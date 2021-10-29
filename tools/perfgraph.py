#!/usr/bin/python
# This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

# Given a profile dump, this tool generates a flame graph based on the stacks listed in the profile
# The result of analysis is a .svg file which can be viewed in a browser

import sys
import svg

class Node(svg.Node):
    def __init__(self):
        svg.Node.__init__(self)
        self.function = ""
        self.source = ""
        self.line = 0
        self.ticks = 0

    def text(self):
        return self.function

    def title(self):
        if self.line > 0:
            return "{}\n{}:{}".format(self.function, self.source, self.line)
        else:
            return self.function

    def details(self, root):
        return "Function: {} [{}:{}] ({:,} usec, {:.1%}); self: {:,} usec".format(self.function, self.source, self.line, self.width, self.width / root.width, self.ticks)

with open(sys.argv[1]) as f:
    dump = f.readlines()

root = Node()

for l in dump:
    ticks, stack = l.strip().split(" ", 1)
    node = root

    for f in reversed(stack.split(";")):
        source, function, line = f.split(",")

        child = node.child(f)
        child.function = function
        child.source = source
        child.line = int(line) if len(line) > 0 else 0

        node = child

    node.ticks += int(ticks)

svg.layout(root, lambda n: n.ticks)
svg.display(root, "Flame Graph", "hot", flip = True)
