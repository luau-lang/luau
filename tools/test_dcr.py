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


class Handler(x.ContentHandler):
    def __init__(self, failList):
        self.currentTest = []
        self.failList = failList  # Set of dotted test names that are expected to fail

        self.results = {}  # {DottedName: TrueIfTheTestPassed}

    def startElement(self, name, attrs):
        if name == "TestSuite":
            self.currentTest.append(attrs["name"])
        elif name == "TestCase":
            self.currentTest.append(attrs["name"])

        elif name == "OverallResultsAsserts":
            if self.currentTest:
                try:
                    failed = 0 != int(attrs["failures"])
                except ValueError:
                    failed = False

                dottedName = ".".join(self.currentTest)
                shouldFail = dottedName in self.failList

                if failed and not shouldFail:
                    print("UNEXPECTED: {} should have passed".format(dottedName))
                elif not failed and shouldFail:
                    print("UNEXPECTED: {} should have failed".format(dottedName))

                self.results[dottedName] = not failed

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

    p = sp.Popen(
        [
            args.path,
            "--reporters=xml",
            "--fflags=true,DebugLuauDeferredConstraintResolution=true",
        ],
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
        print("Updated faillist.txt")

    sys.exit(
        0
        if all(
            not passed == (dottedName in failList)
            for dottedName, passed in handler.results.items()
        )
        else 1
    )

if __name__ == "__main__":
    main()
