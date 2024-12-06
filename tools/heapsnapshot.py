#!/usr/bin/python3
# This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

# Given a Luau heap dump, this tool generates a heap snapshot which can be imported by Chrome's DevTools Memory panel
# To generate a snapshot, use luaC_dump, ideally preceded by luaC_fullgc
# To import in Chrome, ensure the snapshot has the .heapsnapshot extension and go to: Inspect -> Memory -> Load Profile
# A reference for the heap snapshot schema can be found here: https://learn.microsoft.com/en-us/microsoft-edge/devtools-guide-chromium/memory-problems/heap-snapshot-schema

# Usage: python3 heapsnapshot.py luauDump.json heapSnapshot.heapsnapshot

import json
import sys

# Header describing the snapshot format, copied from a real Chrome heap snapshot
snapshotMeta = {
    "node_fields": ["type", "name", "id", "self_size", "edge_count", "trace_node_id", "detachedness"],
    "node_types": [
        ["hidden", "array", "string", "object", "code", "closure", "regexp", "number", "native", "synthetic", "concatenated string", "sliced string", "symbol", "bigint", "object shape"],
        "string", "number", "number", "number", "number", "number"
    ],
    "edge_fields": ["type", "name_or_index", "to_node"],
    "edge_types": [
        ["context", "element", "property", "internal", "hidden", "shortcut", "weak"],
        "string_or_number", "node"
    ],
    "trace_function_info_fields": ["function_id", "name", "script_name", "script_id", "line", "column"],
    "trace_node_fields": ["id", "function_info_index", "count", "size", "children"],
    "sample_fields": ["timestamp_us", "last_assigned_id"],
    "location_fields": ["object_index", "script_id", "line", "column"],
}

# These indices refer to the index in the snapshot's metadata header
nodeTypeToMetaIndex = {type: i for i, type in enumerate(snapshotMeta["node_types"][0])}
edgeTypeToMetaIndex = {type: i for i, type in enumerate(snapshotMeta["edge_types"][0])}

nodeFieldCount = len(snapshotMeta["node_fields"])
edgeFieldCount = len(snapshotMeta["edge_fields"])


def readAddresses(data):
    # Ordered list of addresses to ensure the registry is the first node, and also so we can process nodes in index order
    addresses = []
    addressToNodeIndex = {}

    def addAddress(address):
        assert address not in addressToNodeIndex, f"Address already exists in the snapshot: '{address}'"
        addresses.append(address)
        addressToNodeIndex[address] = len(addresses) - 1

    # The registry is a special case that needs to be either the first or last node to ensure gc "distances" are calculated correctly
    registryAddress = data["roots"]["registry"]
    addAddress(registryAddress)

    for address, obj in data["objects"].items():
        if address == registryAddress:
            continue
        addAddress(address)

    return addresses, addressToNodeIndex


def convertToSnapshot(data):
    addresses, addressToNodeIndex = readAddresses(data)

    # Some notable idiosyncrasies with the heap snapshot format:
    # 1. The snapshot format contains a flat array of nodes and edges. Oddly, edges must reference the "absolute" index of a node's first element after flattening.
    # 2. A node's outgoing edges are implicitly represented by a contiguous block of edges in the edges array which correspond to the node's position
    #    in the nodes array and its edge count. So if the first node has 3 edges, the first 3 edges in the edges array are its edges, and so on.

    nodes = []
    edges = []
    strings = []

    stringToSnapshotIndex = {}

    def getUniqueId(address):
        # TODO: we should hash this to an int32 instead of using the address directly
        # Addresses are hexadecimal strings
        return int(address, 16)

    def addNode(node):
        assert len(node) == nodeFieldCount, f"Expected {nodeFieldCount} fields, got {len(node)}"
        nodes.append(node)

    def addEdge(edge):
        assert len(edge) == edgeFieldCount, f"Expected {edgeFieldCount} fields, got {len(edge)}"
        edges.append(edge)

    def getStringSnapshotIndex(string):
        assert isinstance(string, str), f"'{string}' is not of type string"
        if string not in stringToSnapshotIndex:
            strings.append(string)
            stringToSnapshotIndex[string] = len(strings) - 1
        return stringToSnapshotIndex[string]

    def getNodeSnapshotIndex(address):
        # This is the index of the first element of the node in the flattened nodes array
        return addressToNodeIndex[address] * nodeFieldCount

    for address in addresses:
        obj = data["objects"][address]
        edgeCount = 0

        if obj["type"] == "table":
            # TODO: support weak references
            name = f"Registry ({address})" if address == data["roots"]["registry"] else f"Luau table ({address})"
            if "pairs" in obj:
                for i in range(0, len(obj["pairs"]), 2):
                    key = obj["pairs"][i]
                    value = obj["pairs"][i + 1]
                    if key is None and value is None:
                        # Both the key and value are value types, nothing meaningful to add here
                        continue
                    elif key is None:
                        edgeCount += 1
                        addEdge([edgeTypeToMetaIndex["property"], getStringSnapshotIndex("(Luau table key value type)"), getNodeSnapshotIndex(value)])
                    elif value is None:
                        edgeCount += 1
                        addEdge([edgeTypeToMetaIndex["internal"], getStringSnapshotIndex(f'Luau table key ref: {data["objects"][key]["type"]} ({key})'), getNodeSnapshotIndex(key)])
                    elif data["objects"][key]["type"] == "string":
                        edgeCount += 2
                        # This is a special case where the key is a string, so we can use it as the edge name
                        addEdge([edgeTypeToMetaIndex["property"], getStringSnapshotIndex(data["objects"][key]["data"]), getNodeSnapshotIndex(value)])
                        addEdge([edgeTypeToMetaIndex["internal"], getStringSnapshotIndex(f'Luau table key ref: {data["objects"][key]["type"]} ({key})'), getNodeSnapshotIndex(key)])
                    else:
                        edgeCount += 2
                        addEdge([edgeTypeToMetaIndex["property"], getStringSnapshotIndex(f'{data["objects"][key]["type"]} ({key})'), getNodeSnapshotIndex(value)])
                        addEdge([edgeTypeToMetaIndex["internal"], getStringSnapshotIndex(f'Luau table key ref: {data["objects"][key]["type"]} ({key})'), getNodeSnapshotIndex(key)])
            if "array" in obj:
                for i, element in enumerate(obj["array"]):
                    edgeCount += 1
                    addEdge([edgeTypeToMetaIndex["element"], i, getNodeSnapshotIndex(element)])
            if "metatable" in obj:
                edgeCount += 1
                addEdge([edgeTypeToMetaIndex["internal"], getStringSnapshotIndex(f'metatable ({obj["metatable"]})'), getNodeSnapshotIndex(obj["metatable"])])
            # TODO: consider distinguishing "object" and "array" node types
            addNode([nodeTypeToMetaIndex["object"], getStringSnapshotIndex(name), getUniqueId(address), obj["size"], edgeCount, 0, 0])
        elif obj["type"] == "thread":
            name = f'Luau thread: {obj["source"]}:{obj["line"]} ({address})' if "source" in obj else f"Luau thread ({address})"
            if address == data["roots"]["mainthread"]:
                name += " (main thread)"
            if "env" in obj:
                edgeCount += 1
                addEdge([edgeTypeToMetaIndex["context"], getStringSnapshotIndex(f'env ({obj["env"]})'), getNodeSnapshotIndex(obj["env"])])
            if "stack" in obj:
                for i, frame in enumerate(obj["stack"]):
                    edgeCount += 1
                    addEdge([edgeTypeToMetaIndex["context"], getStringSnapshotIndex(f"callstack[{i}]"), getNodeSnapshotIndex(frame)])
            addNode([nodeTypeToMetaIndex["native"], getStringSnapshotIndex(name), getUniqueId(address), obj["size"], edgeCount, 0, 0])
        elif obj["type"] == "function":
            name = f'Luau function: {obj["name"]} ({address})' if "name" in obj else f"Luau anonymous function ({address})"
            if "env" in obj:
                edgeCount += 1
                addEdge([edgeTypeToMetaIndex["context"], getStringSnapshotIndex(f'env ({obj["env"]})'), getNodeSnapshotIndex(obj["env"])])
            if "proto" in obj:
                edgeCount += 1
                addEdge([edgeTypeToMetaIndex["context"], getStringSnapshotIndex(f'proto ({obj["proto"]})'), getNodeSnapshotIndex(obj["proto"])])
            if "upvalues" in obj:
                for i, upvalue in enumerate(obj["upvalues"]):
                    edgeCount += 1
                    addEdge([edgeTypeToMetaIndex["context"], getStringSnapshotIndex(f"up value ({upvalue})"), getNodeSnapshotIndex(upvalue)])
            addNode([nodeTypeToMetaIndex["closure"], getStringSnapshotIndex(name), getUniqueId(address), obj["size"], edgeCount, 0, 0])
        elif obj["type"] == "upvalue":
            if "object" in obj:
                edgeCount += 1
                addEdge([edgeTypeToMetaIndex["context"], getStringSnapshotIndex(f'upvalue object ({obj["object"]})'), getNodeSnapshotIndex(obj["object"])])
            addNode([nodeTypeToMetaIndex["native"], getStringSnapshotIndex(f"Luau upvalue ({address})"), getUniqueId(address), obj["size"], edgeCount, 0, 0])
        elif obj["type"] == "userdata":
            if "metatable" in obj:
                edgeCount += 1
                addEdge([edgeTypeToMetaIndex["internal"], getStringSnapshotIndex(f'metatable ({obj["metatable"]})'), getNodeSnapshotIndex(obj["metatable"])])
            addNode([nodeTypeToMetaIndex["native"], getStringSnapshotIndex(f"Luau userdata ({address})"), getUniqueId(address), obj["size"], edgeCount, 0, 0])
        elif obj["type"] == "proto":
            name = f'Luau proto: {obj["source"]}:{obj["line"]} ({address})' if "source" in obj else f"Luau proto ({address})"
            if "constants" in obj:
                for constant in obj["constants"]:
                    edgeCount += 1
                    addEdge([edgeTypeToMetaIndex["context"], getStringSnapshotIndex(constant), getNodeSnapshotIndex(constant)])
            if "protos" in obj:
                for proto in obj["protos"]:
                    edgeCount += 1
                    addEdge([edgeTypeToMetaIndex["context"], getStringSnapshotIndex(proto), getNodeSnapshotIndex(proto)])
            addNode([nodeTypeToMetaIndex["code"], getStringSnapshotIndex(name), getUniqueId(address), obj["size"], edgeCount, 0, 0])
        elif obj["type"] == "string":
            addNode([nodeTypeToMetaIndex["string"], getStringSnapshotIndex(obj["data"]), getUniqueId(address), obj["size"], 0, 0, 0])
        elif obj["type"] == "buffer":
            addNode([nodeTypeToMetaIndex["native"], getStringSnapshotIndex(f'buffer ({address})'), getUniqueId(address), obj["size"], 0, 0, 0])
        else:
            raise Exception(f"Unknown object type: '{obj['type']}'")

    return {
        "snapshot": {
            "meta": snapshotMeta,
            "node_count": len(nodes),
            "edge_count": len(edges),
            "trace_function_count": 0,
        },
        # flatten the nodes and edges arrays
        "nodes": [field for node in nodes for field in node],
        "edges": [field for edge in edges for field in edge],
        "trace_function_infos": [],
        "trace_tree": [],
        "samples": [],
        "locations": [],
        "strings": strings,
    }


if __name__ == "__main__":
    luauDump = sys.argv[1]
    heapSnapshot = sys.argv[2]

    with open(luauDump, "r") as file:
        dump = json.load(file)

    snapshot = convertToSnapshot(dump)

    with open(heapSnapshot, "w") as file:
        json.dump(snapshot, file)

    print(f"Heap snapshot written to: '{heapSnapshot}'")
