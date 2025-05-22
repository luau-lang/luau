#!/usr/bin/python3
# This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

import argparse
import os.path
import subprocess as sp
import sys
import xml.sax as x

try:
    import colorama as c
except ImportError:
    class c:
        class Fore:
            RED=''
            RESET=''
            GREEN=''
else:
    c.init()

SCRIPT_PATH = os.path.split(sys.argv[0])[0]
FAIL_LIST_PATH = os.path.join(SCRIPT_PATH, "faillist.txt")


def loadFailList():
    with open(FAIL_LIST_PATH) as f:
        return set(map(str.strip, f.readlines()))


def safeParseInt(i, default=0):
    try:
        return int(i)
    except ValueError:
        return default


def makeDottedName(path):
    return ".".join(path)


class Handler(x.ContentHandler):
    def __init__(self, failList):
        self.currentTest = []
        self.failList = failList  # Set of dotted test names that are expected to fail

        self.results = {}  # {DottedName: TrueIfTheTestPassed}

        self.numSkippedTests = 0

        self.pass_count = 0
        self.fail_count = 0
        self.test_count = 0

        self.crashed_tests = []

    def startElement(self, name, attrs):
        if name == "TestSuite":
            self.currentTest.append(attrs["name"])
        elif name == "TestCase":
            self.currentTest.append(attrs["name"])

        elif name == "OverallResultsAsserts":
            if self.currentTest:
                passed = attrs["test_case_success"] == "true"

                dottedName = makeDottedName(self.currentTest)

                # Sometimes we get multiple XML trees for the same test. All of
                # them must report a pass in order for us to consider the test
                # to have passed.
                r = self.results.get(dottedName, True)
                self.results[dottedName] = r and passed

                self.test_count += 1
                if passed:
                    self.pass_count += 1
                else:
                    self.fail_count += 1

        elif name == "OverallResultsTestCases":
            self.numSkippedTests = safeParseInt(attrs.get("skipped", 0))

        elif name == "Exception":
            if attrs.get("crash") == "true":
                self.crashed_tests.append(makeDottedName(self.currentTest))

    def endElement(self, name):
        if name == "TestCase":
            self.currentTest.pop()

        elif name == "TestSuite":
            self.currentTest.pop()


def print_stderr(*args, **kw):
    print(*args, **kw, file=sys.stderr)


def main():
    parser = argparse.ArgumentParser(
        description="Run Luau.UnitTest with deferred constraint resolution enabled"
    )
    parser.add_argument(
        "path", action="store", help="Path to the Luau.UnitTest executable"
    )
    parser.add_argument(
        "--fflags",
        dest="flags",
        action="store",
        help="Set extra FFlags",
    )
    parser.add_argument(
        "--dump",
        dest="dump",
        action="store_true",
        help="Instead of doing any processing, dump the raw output of the test run.  Useful for debugging this tool.",
    )
    parser.add_argument(
        "--write",
        dest="write",
        action="store_true",
        help="Write a new faillist.txt after running tests.",
    )
    parser.add_argument(
        "--ts",
        dest="suite",
        action="store",
        help="Only run a specific suite."
    )

    parser.add_argument("--randomize", action="store_true", help="Pick a random seed")

    parser.add_argument(
        "--random-seed",
        action="store",
        dest="random_seed",
        type=int,
        help="Accept a specific RNG seed",
    )

    args = parser.parse_args()

    failList = loadFailList()

    flags = "true,LuauSolverV2"
    if args.flags:
        flags += "," + args.flags

    commandLine = [args.path, "--reporters=xml", "--fflags=" + flags]

    if args.random_seed:
        commandLine.append("--random-seed=" + str(args.random_seed))
    elif args.randomize:
        commandLine.append("--randomize")

    if args.suite:
        commandLine.append(f'--ts={args.suite}')

    print_stderr(">", " ".join(commandLine))

    p = sp.Popen(
        commandLine,
        stdout=sp.PIPE,
    )

    assert p.stdout

    handler = Handler(failList)

    if args.dump:
        for line in p.stdout:
            sys.stdout.buffer.write(line)
        return
    else:
        try:
            x.parse(p.stdout, handler)
        except x.SAXParseException as e:
            print_stderr(
                f"XML parsing failed during test {makeDottedName(handler.currentTest)}.  That probably means that the test crashed"
            )
            sys.exit(1)

    p.wait()

    unexpected_fails = 0
    unexpected_passes = 0

    for testName, passed in handler.results.items():
        if passed and testName in failList:
            unexpected_passes += 1
            print_stderr(
                f"UNEXPECTED: {c.Fore.RED}{testName}{c.Fore.RESET} should have failed"
            )
        elif not passed and testName not in failList:
            unexpected_fails += 1
            print_stderr(
                f"UNEXPECTED: {c.Fore.GREEN}{testName}{c.Fore.RESET} should have passed"
            )

    if unexpected_fails or unexpected_passes:
        print_stderr("")
        print_stderr(f"Unexpected fails:  {unexpected_fails}")
        print_stderr(f"Unexpected passes: {unexpected_passes}")

    pass_percent = int(handler.pass_count / handler.test_count * 100)

    print_stderr("")
    print_stderr(
        f"{handler.pass_count} of {handler.test_count} tests passed.  ({pass_percent}%)"
    )
    print_stderr(f"{handler.fail_count} tests failed.")

    if args.write:
        newFailList = sorted(
            (
                dottedName
                for dottedName, passed in handler.results.items()
                if not passed
            ),
            key=str.lower,
        )
        with open(FAIL_LIST_PATH, "w", newline="\n") as f:
            for name in newFailList:
                print(name, file=f)
        print_stderr("Updated faillist.txt")

    if handler.crashed_tests:
        print_stderr()
        for test in handler.crashed_tests:
            print_stderr(
                f"{c.Fore.RED}{test}{c.Fore.RESET} threw an exception and crashed the test process!"
            )

    if handler.numSkippedTests > 0:
        print_stderr(f"{handler.numSkippedTests} test(s) were skipped!")

    ok = (
        not handler.crashed_tests
        and handler.numSkippedTests == 0
        and all(
            not passed == (dottedName in failList)
            for dottedName, passed in handler.results.items()
        )
    )

    if ok:
        print_stderr("Everything in order!")

    sys.exit(0 if ok else 1)


if __name__ == "__main__":
    main()
