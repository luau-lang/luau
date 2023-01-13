#!/usr/bin/python3
# This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

# Given a profile dump, this tool displays top functions based on the stacks listed in the profile

import argparse

class Node:
    def __init__(self):
        self.function = ""
        self.source = ""
        self.line = 0
        self.hier_ticks = 0
        self.self_ticks = 0

    def title(self):
        if self.line > 0:
            return "{} ({}:{})".format(self.function, self.source, self.line)
        else:
            return self.function

argumentParser = argparse.ArgumentParser(description='Display summary statistics from Luau sampling profiler dumps')
argumentParser.add_argument('source_file', type=open)
argumentParser.add_argument('--limit', dest='limit', type=int, default=10, help='Display top N functions')

arguments = argumentParser.parse_args()

dump = arguments.source_file.readlines()

stats = {}
total = 0
total_gc = 0

for l in dump:
    ticks, stack = l.strip().split(" ", 1)
    hier = {}

    for f in reversed(stack.split(";")):
        source, function, line = f.split(",")
        node = stats.setdefault(f, Node())

        node.function = function
        node.source = source
        node.line = int(line) if len(line) > 0 else 0

        if not node in hier:
            node.hier_ticks += int(ticks)
            hier[node] = True

    total += int(ticks)
    node.self_ticks += int(ticks)

    if node.source == "GC":
        total_gc += int(ticks)

if total > 0:
    print(f"Runtime: {total:,} usec ({100.0 * total_gc / total:.2f}% GC)")
    print()
    print("Top functions (self time):")
    for n in sorted(stats.values(), key=lambda node: node.self_ticks, reverse=True)[:arguments.limit]:
        print(f"{n.self_ticks:12,} usec ({100.0 * n.self_ticks / total:.2f}%): {n.title()}")
    print()
    print("Top functions (total time):")
    for n in sorted(stats.values(), key=lambda node: node.hier_ticks, reverse=True)[:arguments.limit]:
        print(f"{n.hier_ticks:12,} usec ({100.0 * n.hier_ticks / total:.2f}%): {n.title()}")
