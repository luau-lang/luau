# This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

import argparse
import os.path
import subprocess as sp
import sys
import xml.sax as x

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


class Handler(x.ContentHandler):
    def __init__(self, failList):
        self.currentTest = []
        self.failList = failList  # Set of dotted test names that are expected to fail

        self.results = {}  # {DottedName: TrueIfTheTestPassed}

        self.numSkippedTests = 0

    def startElement(self, name, attrs):
        if name == "TestSuite":
            self.currentTest.append(attrs["name"])
        elif name == "TestCase":
            self.currentTest.append(attrs["name"])

        elif name == "OverallResultsAsserts":
            if self.currentTest:
                passed = attrs["test_case_success"] == "true"

                dottedName = ".".join(self.currentTest)

                # Sometimes we get multiple XML trees for the same test. All of
                # them must report a pass in order for us to consider the test
                # to have passed.
                r = self.results.get(dottedName, True)
                self.results[dottedName] = r and passed

        elif name == "OverallResultsTestCases":
            self.numSkippedTests = safeParseInt(attrs.get("skipped", 0))

    def endElement(self, name):
        if name == "TestCase":
            self.currentTest.pop()

        elif name == "TestSuite":
            self.currentTest.pop()


def main():
    parser = argparse.ArgumentParser(
        description="Run Luau.UnitTest with deferred constraint resolution enabled"
    )
    parser.add_argument(
        "path", action="store", help="Path to the Luau.UnitTest executable"
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

    args = parser.parse_args()

    failList = loadFailList()

    commandLine = [
        args.path,
        "--reporters=xml",
        "--fflags=true,DebugLuauDeferredConstraintResolution=true",
    ]

    print('>', ' '.join(commandLine), file=sys.stderr)

    p = sp.Popen(
        commandLine,
        stdout=sp.PIPE,
    )

    handler = Handler(failList)

    if args.dump:
        for line in p.stdout:
            sys.stdout.buffer.write(line)
        return
    else:
        x.parse(p.stdout, handler)

    p.wait()

    for testName, passed in handler.results.items():
        if passed and testName in failList:
            print("UNEXPECTED: {} should have failed".format(testName))
        elif not passed and testName not in failList:
            print("UNEXPECTED: {} should have passed".format(testName))

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
        print("Updated faillist.txt", file=sys.stderr)

    if handler.numSkippedTests > 0:
        print(
            "{} test(s) were skipped!  That probably means that a test segfaulted!".format(
                handler.numSkippedTests
            ),
            file=sys.stderr,
        )
        sys.exit(1)

    ok = all(
        not passed == (dottedName in failList)
        for dottedName, passed in handler.results.items()
    )

    if ok:
        print("Everything in order!", file=sys.stderr)

    sys.exit(0 if ok else 1)


if __name__ == "__main__":
    main()
