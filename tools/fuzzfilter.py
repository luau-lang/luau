#!/usr/bin/python3
# This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

# Given a fuzzer binary and a list of crashing programs, this tool collects unique crash reasons and prints reproducers.

import re
import sys
import subprocess

def get_crash_reason(binary, file):
    res = subprocess.run([binary, file], stdout=subprocess.DEVNULL, stderr=subprocess.PIPE)
    if res.returncode == 0:
        print(f"Warning: {binary} {file} returned 0")
        return None
    err = res.stderr.decode("utf-8")

    if (pos := err.find("ERROR: libFuzzer:")) != -1:
        return err[pos:]

    print(f"Warning: {binary} {file} returned unrecognized error {err}")
    return None

def get_crash_fingerprint(reason):
    # Due to ASLR addresses are different every time, so we filter them out
    reason = re.sub(r"0x[0-9a-f]+", "0xXXXX", reason)
    return reason

binary = sys.argv[1]
files = sys.argv[2:]

seen = set()

for index, file in enumerate(files):
    reason = get_crash_reason(binary, file)
    if reason is None:
        continue
    fingerprint = get_crash_fingerprint(reason)
    if fingerprint in seen:
        # print a spinning ASCII wheel to indicate that we're making progress
        print("-\|/"[index % 4] + "\r", end="")
        continue
    seen.add(fingerprint)
    print(f"Reproducer: {binary} {file}")
    print(f"Crash reason: {reason}")
    print()

print(f"Total unique crash reasons: {len(seen)}")