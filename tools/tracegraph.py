#!/usr/bin/python
# This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

# Given a trace event file, this tool generates a flame graph based on the event scopes present in the file
# The result of analysis is a .svg file which can be viewed in a browser

import sys
import svg
import json

class Node(svg.Node):
    def __init__(self):
        svg.Node.__init__(self)
        self.caption = ""
        self.description = ""
        self.ticks = 0

    def text(self):
        return self.caption

    def title(self):
        return self.caption

    def details(self, root):
        return "{} ({:,} usec, {:.1%}); self: {:,} usec".format(self.description, self.width, self.width / root.width, self.ticks)

with open(sys.argv[1]) as f:
    dump = f.read()

root = Node()

# Finish the file
if not dump.endswith("]"):
    dump += "{}]"

data = json.loads(dump)

stacks = {}

for l in data:
    if len(l) == 0:
        continue

    # Track stack of each thread, but aggregate values together
    tid = l["tid"]

    if not tid in stacks:
        stacks[tid] = []
    stack = stacks[tid]

    if l["ph"] == 'B':
        stack.append(l)
    elif l["ph"] == 'E':
        node = root

        for e in stack:
            caption = e["name"]
            description = ''

            if "args" in e:
                for arg in e["args"]:
                    if len(description) != 0:
                        description += ", "

                    description += "{}: {}".format(arg, e["args"][arg])

            child = node.child(caption + description)
            child.caption = caption
            child.description = description

            node = child

        begin = stack[-1]

        ticks = l["ts"] - begin["ts"]
        rawticks = ticks

        # Flame graph requires ticks without children duration
        if "childts" in begin:
            ticks -= begin["childts"]

        node.ticks += int(ticks)

        stack.pop()

        if len(stack):
            parent = stack[-1]

            if "childts" in parent:
                parent["childts"] += rawticks
            else:
                parent["childts"] = rawticks

svg.layout(root, lambda n: n.ticks)
svg.display(root, "Flame Graph", "hot", flip = True)
