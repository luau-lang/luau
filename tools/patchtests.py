#!/usr/bin/python3
# This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

# This code can be used to patch Compiler.test.cpp following bytecode changes, based on error output
import sys
import re

(_, filename) = sys.argv
input = sys.stdin.readlines()

errors = []

# 0: looking for error, 1: looking for replacement, 2: collecting replacement, 3: collecting initial text
state = 0

# parse input into errors[] with the state machine; this is using doctest output and expects multi-line match failures
for line in input:
    if state == 0:
        if sys.platform == "win32":
            match = re.match("[^(]+\((\d+)\): ERROR: CHECK_EQ", line)
        else:
            match = re.match("tests/[^:]+:(\d+): ERROR: CHECK_EQ", line)

        if match:
            error_line = int(match[1])
            state = 1
    elif state == 1:
        if re.match("\s*values: CHECK_EQ\(\s*$", line):
            error_repl = []
            state = 2
        elif re.match("\s*values: CHECK_EQ", line):
            state = 0 # skipping single-line checks since we can't patch them
    elif state == 2:
        if line.strip() == ",":
            error_orig = []
            state = 3
        else:
            error_repl.append(line)
    elif state == 3:
        if line.strip() == ")":
            errors.append((error_line, error_orig, error_repl))
            state = 0
        else:
            error_orig.append(line)

# make sure we fully process each individual check
assert(state == 0)

errors.sort(key = lambda e: e[0])

with open(filename, "r") as fp:
    source = fp.readlines()

# patch source text into result[] using errors[]; we expect every match to appear at or after the line error was reported at
result = []

current = 0
index = 0
target = 0

while index < len(source):
    line = source[index]
    error = errors[current] if current < len(errors) else None

    if error:
        target = error[0] if sys.platform != "win32" else error[0] - len(error[1]) - 1

    if not error or index < target or line != error[1][0]:
        result.append(line)
        index += 1
    else:
        # validate that the patch has a complete match in source text
        for v in range(len(error[1])):
            assert(source[index + v] == error[1][v])

        result += error[2]
        index += len(error[1])
        current += 1

# make sure we patch all errors
assert(current == len(errors))

with open(filename, "w") as fp:
    fp.writelines(result)
