#!/usr/bin/python

import argparse
import difflib
import enum
import os
import os.path
import subprocess
import sys

SUITES = ["interpreter", "prettyprinter"]
IN_FILE_NAME = "in.lua"
OUT_FILE_NAME = "out.txt"
SUITE_EXE_NAMES = {
    "interpreter": "Interpreter",
    "prettyprinter": "PrettyPrinter",
}

SUITE_ENTRY_POINTS = {
    "interpreter": "Interpreter.agda",
    "prettyprinter": "PrettyPrinter.agda",
}

SUITE_ROOTS = {
    "interpreter": "Tests/Interpreter",
    "prettyprinter": "Tests/PrettyPrinter",
}

class TestResultStatus(enum.Enum):
    CLI_ERROR = 0
    EXE_ERROR = 1
    DIFF_ERROR = 2
    SUCCESS = 3
    WROTE_NEW = 4

class DiffFailure:
    def __init__(self, expected, actual):
        self.expected = expected
        self.actual = actual
    
    def diff_text(self):
        diff_generator = difflib.context_diff(self.expected.splitlines(), self.actual.splitlines(), fromfile="expected", tofile="actual", n=3)
        return "".join(diff_generator)
    
    def diff_html(self):
        differ = difflib.HtmlDiff(tabsize=4)
        return differ.make_file(self.expected.splitlines(), self.actual.splitlines(), fromdesc="Expected", todesc="Actual", context=True, numlines=5)

class TestCaseResult:
    def __init__(self, suite, case, status, details):
        self.suite = suite
        self.case = case
        self.status = status
        self.details = details
    
    def did_pass(self):
        return self.status == TestResultStatus.SUCCESS or self.status == TestResultStatus.WROTE_NEW
    
    def to_string(self):
        prefix = f"[{self.suite}/{self.case}]: "
        if self.status == TestResultStatus.CLI_ERROR:
            return f"{prefix}CLI ERROR: {self.details}"
        elif self.status == TestResultStatus.EXE_ERROR:
            return f"{prefix}EXE ERROR: {self.details}"
        elif self.status == TestResultStatus.DIFF_ERROR:
            text_diff = self.details.diff_text()
            return f"{prefix}FAILED:\n{text_diff}"
        elif self.status == TestResultStatus.SUCCESS:
            return f"{prefix}SUCCEEDED"
        elif self.status == TestResultStatus.WROTE_NEW:
            return f"{prefix}WROTE NEW RESULT"
    
    def write_artifact(self, artifact_root):
        if self.status != TestResultStatus.DIFF_ERROR:
            return

        filename = f"{self.suite}-{self.case}.out.html"
        path = os.path.join(artifact_root, filename)
        html = self.details.diff_html()
        with open(path, "w") as file:
            file.write(html)

parser = argparse.ArgumentParser(description="Runs prototype test cases")
parser.add_argument("--luau-cli", "-l", dest="cli_location", required=True, help="The location of luau-cli")
parser.add_argument("--root", "-r", dest="prototype_root", required=False, default=os.getcwd(), help="The root of the prototype")
parser.add_argument("--build", "-b", dest="build", action="store_true", default=True, help="Whether to automatically build required test binaries")
parser.add_argument("--suite", "-s", dest="suites", action="append", default=[], choices=SUITES, help="Which test suites to run")
parser.add_argument("--case", "-c", dest="cases", action="append", default=[], help="Which test cases to run")
parser.add_argument("--accept-new-output", "-a", dest="snapshot", action="store_true", default=False, help="Whether to write the new output to files, instead of diffing against it")
parser.add_argument("--write-diff-failures", dest="write_diffs", action="store_true", default=False, help="Whether to write test failure diffs to files")
parser.add_argument("--diff-failure-location", dest="diff_location", default=None, help="Where to write diff failure files to")

def build_suite(root, suite):
    entry_point = SUITE_ENTRY_POINTS.get(suite)
    if entry_point is None:
        return (False, "Invalid suite")
    
    result = subprocess.run(["~/.cabal/bin/agda", "--compile", entry_point], shell=True, cwd=root, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
    if result.returncode == 0:
        return (True, None)
    else:
        return (False, result.stdout)

def run_test(in_path, out_path, cli_path, exe_path, snapshot):
    cli_result = subprocess.run([cli_path, in_path], capture_output=True)
    if cli_result.returncode != 0:
        return (TestResultStatus.CLI_ERROR, f"CLI error: {cli_result.stderr}")
    
    exe_result = subprocess.run(exe_path, input=cli_result.stdout, capture_output=True)
    if exe_result.returncode != 0:
        return (TestResultStatus.EXE_ERROR, f"Executable error; stdout:{exe_result.stdout}\n\nstderr: {exe_result.stderr}")
    actual_result = exe_result.stdout.decode("utf-8")

    if snapshot:
        with open(out_path, "w") as out_file:
            out_file.write(actual_result)
            return (TestResultStatus.WROTE_NEW, None)
    else:
        with open(out_path, "r") as out_file:
            expected_result = out_file.read()

            if expected_result != actual_result:
                return (TestResultStatus.DIFF_ERROR, DiffFailure(expected_result, actual_result))
    
    return (TestResultStatus.SUCCESS, None)

def should_run_case(case_name, filters):
    if len(filters) == 0:
        return True

    return any([f in case_name for f in filters])

def run_test_suite(args, suite, suite_root, suite_exe):
    results = []

    for entry in os.listdir(suite_root):
        if not should_run_case(entry, args.cases):
            continue

        case_path = os.path.join(suite_root, entry)
        if os.path.isdir(case_path):
            in_path = os.path.join(case_path, IN_FILE_NAME)
            out_path = os.path.join(case_path, OUT_FILE_NAME)

            if not os.path.exists(in_path) or not os.path.exists(out_path):
                continue
            
            status, details = run_test(in_path, out_path, args.cli_location, suite_exe, args.snapshot)
            result = TestCaseResult(suite, entry, status, details)
            results.append(result)

    return results

def main():
    args = parser.parse_args()

    suites = args.suites if len(args.suites) > 0 else SUITES
    root = os.path.abspath(args.prototype_root)

    if args.build:
        for suite in suites:
            success, reason = build_suite(root, suite)

            if not success:
                print(f"Error building executable for test suite {suite}:\n{reason}")
                sys.exit(1)
            else:
                print(f"Built executable for test suite {suite} successfully.")
    
    failed = False
    for suite in suites:
        suite_root = os.path.join(root, SUITE_ROOTS.get(suite))
        suite_exe = os.path.join(root, SUITE_EXE_NAMES.get(suite))
        print(f"Running test suite {suite}...")
        results = run_test_suite(args, suite, suite_root, suite_exe)

        passed = 0
        total = len(results)

        for result in results:
            if result.did_pass():
                passed += 1
            else:
                failed = True

        print(f"Suite {suite} [{passed} / {total} passed]:")
        for result in results:
            print(result.to_string())

            if args.write_diffs:
                result.write_artifact(args.diff_location)
    
    if failed:
        sys.exit(1)

if __name__ == "__main__":
    main()
