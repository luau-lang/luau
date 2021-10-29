#!/usr/bin/python
# This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

# Given a heap snapshot, this tool gathers basic statistics about the allocated objects
# To generate a snapshot, use luaC_dump, ideally preceded by luaC_fullgc

import json
import sys
from collections import defaultdict

def updatesize(d, k, s):
    oc, os = d.get(k, (0, 0))
    d[k] = (oc + 1, os + s)

def sortedsize(p):
    return sorted(p, key = lambda s: s[1][1], reverse = True)

with open(sys.argv[1]) as f:
    dump = json.load(f)
    heap = dump["objects"]

type_addr = next((addr for addr,obj in heap.items() if obj["type"] == "string" and obj["data"] == "__type"), None)

size_type = {}
size_udata = {}
size_category = {}

for addr, obj in heap.items():
    updatesize(size_type, obj["type"], obj["size"])

    if obj.get("cat") != None:
        updatesize(size_category, str(obj["cat"]), obj["size"])

    if obj["type"] == "userdata" and "metatable" in obj:
        metatable = heap[obj["metatable"]]
        pairs = metatable.get("pairs", [])
        typemt = "unknown"
        for i in range(0, len(pairs), 2):
            if type_addr and pairs[i] == type_addr and pairs[i + 1] and heap[pairs[i + 1]]["type"] == "string":
                typemt = heap[pairs[i + 1]]["data"]
        updatesize(size_udata, typemt, obj["size"])

print("objects by type:")
for type, (count, size) in sortedsize(size_type.items()):
    print(type.ljust(10), str(size).rjust(8), "bytes", str(count).rjust(5), "objects")

print()

print("userdata by __type:")
for type, (count, size) in sortedsize(size_udata.items()):
    print(type.ljust(20), str(size).rjust(8), "bytes", str(count).rjust(5), "objects")

if len(size_category) != 0:
    print()

    print("objects by category:")
    for type, (count, size) in sortedsize(size_category.items()):
        name = dump["stats"]["categories"][type]["name"]
        print(name.ljust(30), str(size).rjust(8), "bytes", str(count).rjust(5), "objects")
