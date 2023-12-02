#!/usr/bin/python3
# This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

# Given a fuzzer binary and a list of crashing programs, this tool collects unique crash reasons and prints reproducers.

import argparse
import multiprocessing
import os
import re
import subprocess
import sys


def is_crash(reproducer_name: str) -> bool:
    return reproducer_name.startswith("crash-") or reproducer_name.startswith("oom-")


class Reproducer:
    def __init__(self, file, reason, fingerprint):
        self.file = file
        self.reason = reason
        self.fingerprint = fingerprint


def get_crash_reason(binary, file, remove_passing):
    res = subprocess.run(
        [binary, file], stdout=subprocess.DEVNULL, stderr=subprocess.PIPE)

    if res.returncode == 0:
        if remove_passing:
            print(f"Warning: {binary} {file} returned 0; removing from result set.", file=sys.stderr)
            os.remove(file)
        else:
            print(f"Warning: {binary} {file} returned 0", file=sys.stderr)

        return None

    err = res.stderr.decode("utf-8")

    if (pos := err.find("ERROR: AddressSanitizer:")) != -1:
        return err[pos:]

    if (pos := err.find("ERROR: libFuzzer:")) != -1:
        return err[pos:]

    print(f"Warning: {binary} {file} returned unrecognized error {err} with exit code {res.returncode}", file=sys.stderr)
    return None


def get_crash_fingerprint(reason):
    # Due to ASLR addresses are different every time, so we filter them out
    reason = re.sub(r"0x[0-9a-f]+", "0xXXXX", reason)
    return reason


parser = argparse.ArgumentParser()
parser.add_argument("binary")
parser.add_argument("files", action="append", default=[])
parser.add_argument("--remove-duplicates", action="store_true")
parser.add_argument("--remove-passing", action="store_true")
parser.add_argument("--workers", action="store", default=1, type=int)
parser.add_argument("--verbose", "-v", action="count", default=0, dest="verbosity")

args = parser.parse_args()

def process_file(file):
    reason = get_crash_reason(args.binary, file, args.remove_passing)
    if reason is None:
        return None

    fingerprint = get_crash_fingerprint(reason)
    return Reproducer(file, reason, fingerprint)


filter_targets = []
if len(args.files) == 1:
    for root, dirs, files in os.walk(args.files[0]):
        for file in files:
            if not is_crash(file):
                continue

            filter_targets.append(os.path.join(root, file))
else:
    filter_targets = args.files

if __name__ == "__main__":
    multiprocessing.freeze_support()

    with multiprocessing.Pool(processes = args.workers) as pool:
        print(f"Processing {len(filter_targets)} reproducers across {args.workers} workers.")
        reproducers = [r for r in pool.map(process_file, filter_targets) if r is not None]

        seen = set()
        for index, reproducer in enumerate(reproducers):
            if reproducer.fingerprint in seen:
                if sys.stdout.isatty():
                    print("-\|/"[index % 4], end="\r")

                if args.remove_duplicates:
                    if args.verbosity >= 1:
                        print(f"Removing duplicate reducer {reproducer.file}.")
                    os.remove(reproducer.file)

                continue

            seen.add(reproducer.fingerprint)
            if args.verbosity >= 2:
                print(f"Reproducer: {args.binary} {reproducer.file}")
                print(f"Output: {reproducer.reason}")
        
        print(f"Total unique crashes: {len(seen)}")
        if args.remove_duplicates:
            print(f"Duplicate reproducers have been removed.")
