#!/usr/bin/python
# This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

# Given a profile dump, this tool generates a flame graph based on the stacks listed in the profile
# The result of analysis is a .svg file which can be viewed in a browser

import sys
import svg
import argparse
import json

argumentParser = argparse.ArgumentParser(description='Generate flamegraph SVG from Luau sampling profiler dumps')
argumentParser.add_argument('source_file', type=open)
argumentParser.add_argument('--json', dest='useJson',action='store_const',const=1,default=0,help='Parse source_file as JSON')

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


def nodeFromCallstackListFile(source_file):
    dump = source_file.readlines()
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

    return root


def nodeFromJSONbject(node, key, obj):
    source, function, line = key.split(",")

    node.function = function
    node.source = source
    node.line = int(line) if len(line) > 0 else 0

    node.ticks = obj['Duration']

    for key, obj in obj['Children'].items():
        nodeFromJSONbject(node.child(key), key, obj)

    return node


def nodeFromJSONFile(source_file):
    dump = json.load(source_file)

    root = Node()

    for key, obj in dump['Children'].items():
        nodeFromJSONbject(root.child(key), key, obj)

    return root


arguments = argumentParser.parse_args()

if arguments.useJson:
    root = nodeFromJSONFile(arguments.source_file)
else:
    root = nodeFromCallstackListFile(arguments.source_file)



svg.layout(root, lambda n: n.ticks)
svg.display(root, "Flame Graph", "hot", flip = True)
