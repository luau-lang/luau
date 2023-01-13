#!/usr/bin/python3
# This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

# Given a profile dump, this tool generates a flame graph based on the stacks listed in the profile
# The result of analysis is a .svg file which can be viewed in a browser

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


def getDuration(nodes, nid):
    node = nodes[nid - 1]
    total = node['TotalDuration']

    for cid in node['NodeIds']:
        total -= nodes[cid - 1]['TotalDuration']

    return total

def getFunctionKey(fn):
    return fn['Source'] + "," + fn['Name'] + "," + str(fn['Line'])

def recursivelyBuildNodeTree(nodes, functions, parent, fid, nid):
    ninfo = nodes[nid - 1]
    finfo = functions[fid - 1]

    child = parent.child(getFunctionKey(finfo))
    child.source = finfo['Source']
    child.function = finfo['Name']
    child.line = int(finfo['Line']) if finfo['Line'] > 0 else 0

    child.ticks = getDuration(nodes, nid)

    assert(len(ninfo['FunctionIds']) == len(ninfo['NodeIds']))

    for i in range(0, len(ninfo['FunctionIds'])):
        recursivelyBuildNodeTree(nodes, functions, child, ninfo['FunctionIds'][i], ninfo['NodeIds'][i])

    return

def nodeFromJSONV2(dump):
    assert(dump['Version'] == 2)

    nodes = dump['Nodes']
    functions = dump['Functions']
    categories = dump['Categories']

    root = Node()

    for category in categories:
        nid = category['NodeId']
        node = nodes[nid - 1]
        name = category['Name']

        child = root.child(name)
        child.function = name
        child.ticks = getDuration(nodes, nid)

        assert(len(node['FunctionIds']) == len(node['NodeIds']))

        for i in range(0, len(node['FunctionIds'])):
            recursivelyBuildNodeTree(nodes, functions, child, node['FunctionIds'][i], node['NodeIds'][i])

    return root

def getDurationV1(obj):
    total = obj['TotalDuration']

    if 'Children' in obj:
        for key, obj in obj['Children'].items():
            total -= obj['TotalDuration']

    return total


def nodeFromJSONObject(node, key, obj):
    source, function, line = key.split(",")

    node.function = function
    node.source = source
    node.line = int(line) if len(line) > 0 else 0

    node.ticks = getDurationV1(obj)

    if 'Children' in obj:
        for key, obj in obj['Children'].items():
            nodeFromJSONObject(node.child(key), key, obj)

    return node

def nodeFromJSONV1(dump):
    assert(dump['Version'] == 1)
    root = Node()

    if 'Children' in dump:
        for key, obj in dump['Children'].items():
            nodeFromJSONObject(root.child(key), key, obj)

    return root

def nodeFromJSONFile(source_file):
    dump = json.load(source_file)

    if dump['Version'] == 2:
        return nodeFromJSONV2(dump)
    elif dump['Version'] == 1:
        return nodeFromJSONV1(dump)

    return Node()


arguments = argumentParser.parse_args()

if arguments.useJson:
    root = nodeFromJSONFile(arguments.source_file)
else:
    root = nodeFromCallstackListFile(arguments.source_file)



svg.layout(root, lambda n: n.ticks)
svg.display(root, "Flame Graph", "hot", flip = True)
