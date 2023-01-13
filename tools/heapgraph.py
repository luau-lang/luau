#!/usr/bin/python3
# This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

# Given two heap snapshots (A & B), this tool performs reachability analysis on new objects allocated in B
# This is useful to find memory leaks - reachability analysis answers the question "why is this set of objects not freed"
# This tool can also be ran with just one snapshot, in which case it displays all allocated objects
# The result of analysis is a .svg file which can be viewed in a browser
# To generate these dumps, use luaC_dump, ideally preceded by luaC_fullgc

import argparse
import json
import sys
import svg

argumentParser = argparse.ArgumentParser(description='Luau heap snapshot analyzer')

argumentParser.add_argument('--split', dest = 'split', type = str, default = 'none', help = 'Perform additional root split using memory categories', choices = ['none', 'custom', 'all'])

argumentParser.add_argument('snapshot')
argumentParser.add_argument('snapshotnew', nargs='?')

arguments = argumentParser.parse_args()

class Node(svg.Node):
    def __init__(self):
        svg.Node.__init__(self)
        self.size = 0
        self.count = 0
        # data for memory category filtering
        self.objects = []
        self.categories = set()

    def text(self):
        return self.name

    def title(self):
        return self.name

    def details(self, root):
        return "{} ({:,} bytes, {:.1%}); self: {:,} bytes in {:,} objects".format(self.name, self.width, self.width / root.width, self.size, self.count)

def getkey(heap, obj, key):
    pairs = obj.get("pairs", [])
    for i in range(0, len(pairs), 2):
        if pairs[i] and heap[pairs[i]]["type"] == "string" and heap[pairs[i]]["data"] == key:
            if pairs[i + 1] and heap[pairs[i + 1]]["type"] == "string":
                return heap[pairs[i + 1]]["data"]
            else:
                return None
    return None

# load files
if arguments.snapshotnew == None:
    dumpold = None
    with open(arguments.snapshot) as f:
        dump = json.load(f)
else:
    with open(arguments.snapshot) as f:
        dumpold = json.load(f)
    with open(arguments.snapshotnew) as f:
        dump = json.load(f)

heap = dump["objects"]

# reachability analysis: how much of the heap is reachable from roots?
visited = set()
queue = []
offset = 0
root = Node()

for name, addr in dump["roots"].items():
    queue.append((addr, root.child(name)))

while offset < len(queue):
    addr, node = queue[offset]
    offset += 1
    if addr in visited:
        continue

    visited.add(addr)
    obj = heap[addr]

    if not dumpold or not addr in dumpold["objects"]:
        node.count += 1
        node.size += obj["size"]
        node.objects.append(obj)

    if obj["type"] == "table":
        pairs = obj.get("pairs", [])
        weakkey = False
        weakval = False

        if "metatable" in obj:
            modemt = getkey(heap, heap[obj["metatable"]], "__mode")
            if modemt:
                weakkey = "k" in modemt
                weakval = "v" in modemt

        for i in range(0, len(pairs), 2):
            key = pairs[i+0]
            val = pairs[i+1]
            if key and heap[key]["type"] == "string":
                # string keys are always strong
                queue.append((key, node))
                if val and not weakval:
                    queue.append((val, node.child(heap[key]["data"])))
            else:
                if key and not weakkey:
                    queue.append((key, node))
                if val and not weakval:
                    queue.append((val, node))

        for a in obj.get("array", []):
            queue.append((a, node))
        if "metatable" in obj:
            queue.append((obj["metatable"], node.child("__meta")))
    elif obj["type"] == "function":
        queue.append((obj["env"], node.child("__env")))

        source = ""
        if "proto" in obj:
            proto = heap[obj["proto"]]
            if "source" in proto:
                source = proto["source"]

        if "proto" in obj:
            queue.append((obj["proto"], node.child("__proto")))
        for a in obj.get("upvalues", []):
            queue.append((a, node.child(source)))
    elif obj["type"] == "userdata":
        if "metatable" in obj:
            queue.append((obj["metatable"], node.child("__meta")))
    elif obj["type"] == "thread":
        queue.append((obj["env"], node.child("__env")))
        stack = obj.get("stack")
        stacknames = obj.get("stacknames", [])
        stacknode = node.child("__stack")
        framenode = None
        for i in range(len(stack)):
            name = stacknames[i] if stacknames else None
            if name and name.startswith("frame:"):
                framenode = stacknode.child(name[6:])
                name = None
            queue.append((stack[i], framenode.child(name) if framenode and name else framenode or stacknode))
    elif obj["type"] == "proto":
        for a in obj.get("constants", []):
            queue.append((a, node))
        for a in obj.get("protos", []):
            queue.append((a, node))
    elif obj["type"] == "upvalue":
        if "object" in obj:
            queue.append((obj["object"], node))

def annotateContainedCategories(node, start):
    for obj in node.objects:
        if obj["cat"] < start:
            obj["cat"] = 0

        node.categories.add(obj["cat"])

    for child in node.children.values():
        annotateContainedCategories(child, start)

        for cat in child.categories:
            node.categories.add(cat)

def filteredTreeForCategory(node, category):
    children = {}

    for c in node.children.values():
        if category in c.categories:
            filtered = filteredTreeForCategory(c, category)

            if filtered:
                children[filtered.name] = filtered

    if len(children):
        result = Node()
        result.name = node.name

        # re-count the objects with the correct category that we have
        for obj in node.objects:
            if obj["cat"] == category:
                result.count += 1
                result.size += obj["size"]

        result.children = children
        return result
    else:
        result = Node()
        result.name = node.name

        # re-count the objects with the correct category that we have
        for obj in node.objects:
            if obj["cat"] == category:
                result.count += 1
                result.size += obj["size"]

        if result.count != 0:
            return result

    return None

def splitIntoCategories(root):
    result = Node()

    for i in range(0, 256):
        filtered = filteredTreeForCategory(root, i)

        if filtered:
            name = dump["stats"]["categories"][str(i)]["name"]

            filtered.name = name
            result.children[name] = filtered

    return result

if dump["stats"].get("categories") and arguments.split != 'none':
    if arguments.split == 'custom':
        annotateContainedCategories(root, 128)
    else:
        annotateContainedCategories(root, 0)

    root = splitIntoCategories(root)

svg.layout(root, lambda n: n.size)
svg.display(root, "Memory Graph", "cold")
