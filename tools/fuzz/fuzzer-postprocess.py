#!/usr/bin/python3
# This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

import argparse
import jinja2
import multiprocessing
import os
import shutil
import subprocess
import sys


def is_crash(reproducer_name: str) -> bool:
    return reproducer_name.startswith("crash-") or reproducer_name.startswith("oom-")


class CrashReport:
    def __init__(self, args, crash_id):
        self.id = crash_id
        self.args = args
        self.crash_root = os.path.join(args.output_directory, crash_id)

    def trace(self) -> str:
        trace_path = os.path.join(self.crash_root, "trace.txt")

        if os.path.exists(trace_path):
            with open(os.path.join(self.crash_root, "trace.txt"), "r") as trace_file:
                return trace_file.read()
        else:
            return None

    def modules(self) -> str:
        with open(os.path.join(self.crash_root, "modules.txt"), "r") as modules_file:
            return modules_file.read()

    def artifact_link(self) -> str:
        return f"{self.args.artifact_root}/{self.id}/minimized_reproducer"


class MetaValue:
    def __init__(self, name, value):
        self.name = name
        self.value = value
        self.link = None


def minimize_crash(args, reproducer, workdir):
    if not is_crash(os.path.basename(reproducer)):
        # Not actually a crash, so no minimization is actually possible.
        return

    print(
        f"Minimizing reproducer {os.path.basename(reproducer)} for {args.minimize_for} seconds.")
    
    reproducer_absolute = os.path.abspath(reproducer)

    artifact = os.path.join(workdir, "minimized_reproducer")
    minimize_result = subprocess.run([args.executable, "-detect_leaks=0", "-minimize_crash=1",
                                        f"-exact_artifact_path={artifact}", f"-max_total_time={args.minimize_for}", reproducer_absolute], cwd=workdir, stdout=sys.stdout if args.verbose else subprocess.DEVNULL, stderr=sys.stderr if args.verbose else subprocess.DEVNULL)

    if minimize_result.returncode != 0:
        print(
            f"Minimize process exited with code {minimize_result.returncode}; minimization failed.")
        return

    if os.path.exists(artifact):
        print(
            f"Minimized {os.path.basename(reproducer)} from {os.path.getsize(reproducer)} bytes to {os.path.getsize(artifact)}.")


def process_crash(args, reproducer):
    crash_id = os.path.basename(reproducer)
    crash_output = os.path.join(args.output_directory, crash_id)
    print(f"Processing reproducer {crash_id}.")

    print(f"Output will be stored in {crash_output}.")
    if os.path.exists(crash_output):
        print(f"Contents of {crash_output} will be discarded.")
        shutil.rmtree(crash_output, ignore_errors=True)

    os.makedirs(crash_output)
    shutil.copyfile(reproducer, os.path.join(crash_output, "original_reproducer"))
    shutil.copyfile(reproducer, os.path.join(
        crash_output, "minimized_reproducer"))

    minimize_crash(args, reproducer, crash_output)

    if is_crash(crash_id):
        trace_result = subprocess.run([args.executable, os.path.join(
            crash_output, "minimized_reproducer")], stdout=subprocess.PIPE, stderr=subprocess.STDOUT, text=True)
        trace_text = trace_result.stdout

        with open(os.path.join(crash_output, "trace.txt"), "w") as trace_file:
            trace_file.write(trace_text)

    modules_result = subprocess.run([args.prototest, os.path.join(
        crash_output, "minimized_reproducer"), "-detect_leaks=0", "-verbosity=0"], stdout=subprocess.PIPE, stderr=subprocess.STDOUT, text=True)
    modules_text = modules_result.stdout

    module_index_of = modules_text.index("Module")
    modules_text = modules_text[module_index_of:]

    with open(os.path.join(crash_output, "modules.txt"), "w") as modules_file:
        modules_file.write(modules_text)

    return CrashReport(args, crash_id)


def process_crashes(args):
    crash_names = sorted(os.listdir(args.source_directory))
    with multiprocessing.Pool(args.workers) as pool:
        crashes = [(args, os.path.join(args.source_directory, c)) for c in crash_names]
        crashes = pool.starmap(process_crash, crashes)
        print(f"Processed {len(crashes)} crashes.")
        return crashes


def generate_report(crashes, meta):
    env = jinja2.Environment(
        loader=jinja2.PackageLoader("fuzzer-postprocess"),
        autoescape=jinja2.select_autoescape()
    )

    template = env.get_template("index.html")
    with open("fuzz-report.html", "w") as report_file:
        report_file.write(template.render(
            crashes=crashes,
            meta=meta,
        ))


def __main__():
    parser = argparse.ArgumentParser()
    parser.add_argument("--source_directory", required=True)
    parser.add_argument("--output_directory", required=True)
    parser.add_argument("--executable", required=True)
    parser.add_argument("--prototest", required=True)
    parser.add_argument("--minimize_for", required=True)
    parser.add_argument("--artifact_root", required=True)
    parser.add_argument("--verbose", "-v", action="store_true")
    parser.add_argument("--workers", action="store", type=int, default=4)
    meta_group = parser.add_argument_group(
        "metadata", description="Report metadata to attach.")
    meta_group.add_argument("--meta.values", nargs="*",
                            help="Any metadata to attach, in the form name=value. Multiple values may be specified.", dest="metadata_values", default=[])
    meta_group.add_argument("--meta.urls", nargs="*",
                            help="URLs to attach to metadata, in the form name=url. Multiple values may be specified. A value must also be specified with --meta.values.", dest="metadata_urls", default=[])
    args = parser.parse_args()

    meta_values = dict()
    for pair in args.metadata_values:
        components = pair.split("=", 1)
        name = components[0]
        value = components[1]

        meta_values[name] = MetaValue(name, value)

    for pair in args.metadata_urls:
        components = pair.split("=", 1)
        name = components[0]
        url = components[1]

        if name in meta_values:
            meta_values[name].link = url
        else:
            print(f"Metadata {name} has URL {url} but no value specified.")

    meta_values = sorted(list(meta_values.values()), key=lambda x: x.name)

    crashes = process_crashes(args)
    generate_report(crashes, meta_values)


if __name__ == "__main__":
    __main__()
