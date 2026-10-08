local function prequire(name) local success, result = pcall(require, name); return success and result end
local bench = script and require(script.Parent.bench_support) or prequire("bench_support") or require("../../bench_support")

local block = require("./markdown-dir/block")
local render = require("./markdown-dir/render")

function test()

-- Markdown benchmark: parse and render a large embedded document repeatedly

local document = [[
# Introduction to Computing

This is a **comprehensive** guide to *modern computing*. It covers everything
from basic algorithms to advanced system design.

## Chapter 1: Algorithms

Algorithms are the foundation of computer science. Here are some key concepts:

- Sorting algorithms (quicksort, mergesort, heapsort)
- Search algorithms (binary search, BFS, DFS)
- Graph algorithms (Dijkstra's, A*, Floyd-Warshall)
- Dynamic programming

### 1.1 Sorting

The most common sorting algorithms have O(n log n) complexity:

```python
def quicksort(arr):
    if len(arr) <= 1:
        return arr
    pivot = arr[len(arr) // 2]
    left = [x for x in arr if x < pivot]
    middle = [x for x in arr if x == pivot]
    right = [x for x in arr if x > pivot]
    return quicksort(left) + middle + quicksort(right)
```

> "The art of programming is the art of organizing complexity."
> — Edsger W. Dijkstra

### 1.2 Searching

Binary search is an efficient algorithm for finding items in a **sorted** list.
It works by repeatedly dividing the search interval in half.

1. Compare the target with the middle element
2. If target matches, we're done
3. If target is less, search the left half
4. If target is greater, search the right half

The time complexity is O(log n), which is significantly better than
linear search's O(n) for large datasets.

## Chapter 2: Data Structures

Data structures organize and store data efficiently. Common structures include:

| Structure | Insert | Delete | Search |
|-----------|--------|--------|--------|
| Array     | O(n)   | O(n)   | O(n)   |
| Hash Map  | O(1)   | O(1)   | O(1)   |
| BST       | O(log n)| O(log n)| O(log n)|
| Linked List| O(1)  | O(1)   | O(n)   |

### 2.1 Trees

Trees are hierarchical data structures. A **binary search tree** (BST) maintains
the invariant that for every node:

- All values in the *left* subtree are less than the node
- All values in the *right* subtree are greater than the node

This makes searching very efficient: `O(log n)` in the average case.

### 2.2 Hash Tables

Hash tables provide `O(1)` average-case lookups by using a hash function
to map keys to bucket indices. Collision resolution strategies include:

1. Chaining (linked lists at each bucket)
2. Open addressing (linear probing, quadratic probing)
3. Robin Hood hashing
4. Cuckoo hashing

---

## Chapter 3: Systems Design

Building reliable distributed systems requires understanding of:

- **Consistency** models (strong, eventual, causal)
- **Availability** guarantees (SLAs, redundancy)
- **Partition** tolerance (network failures)

According to the [CAP theorem](https://en.wikipedia.org/wiki/CAP_theorem),
a distributed system can only provide two of these three guarantees
simultaneously.

### 3.1 Load Balancing

Load balancers distribute incoming requests across multiple servers.
Common algorithms include:

- Round Robin
- Least Connections
- IP Hash
- Weighted Round Robin

Here's a simple load balancer configuration:

```nginx
upstream backend {
    server backend1.example.com weight=5;
    server backend2.example.com;
    server backend3.example.com backup;
}
```

### 3.2 Caching

Caching is ***crucial*** for performance. Key considerations:

> Caching reduces latency and database load, but introduces
> complexity around cache invalidation.
>
> "There are only two hard things in Computer Science:
> cache invalidation and naming things." — Phil Karlton

Cache eviction policies:

1. LRU (Least Recently Used)
2. LFU (Least Frequently Used)
3. FIFO (First In, First Out)
4. TTL-based expiration

## Chapter 4: Programming Languages

Different paradigms serve different purposes:

### 4.1 Functional Programming

Functional programming emphasizes *immutability* and *pure functions*.
Key concepts include:

- First-class functions
- Higher-order functions (`map`, `filter`, `reduce`)
- Pattern matching
- Algebraic data types

```haskell
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smaller = quicksort [a | a <- xs, a <= x]
        bigger  = quicksort [a | a <- xs, a > x]
    in smaller ++ [x] ++ bigger
```

### 4.2 Object-Oriented Programming

OOP organizes code around **objects** that encapsulate data and behavior.
The four pillars are:

1. **Encapsulation** — hiding internal state
2. **Inheritance** — code reuse through hierarchies
3. **Polymorphism** — treating objects uniformly
4. **Abstraction** — simplifying complex reality

### 4.3 Concurrency

Modern systems must handle concurrent operations. Approaches include:

- Threads and locks
- Actor model (Erlang, Akka)
- CSP (Go channels)
- Async/await (JavaScript, Python, Rust)

---

## Chapter 5: Best Practices

Writing good code requires discipline:

- Write **tests** before implementation (TDD)
- Keep functions *small* and focused
- Use meaningful variable names
- Document `public APIs` thoroughly
- Review code with peers

> "Any fool can write code that a computer can understand.
> Good programmers write code that humans can understand."
> — Martin Fowler

### Summary

This guide covered the fundamentals of:

1. Algorithms and their complexity
2. Core data structures
3. Distributed systems design
4. Programming language paradigms
5. Software engineering best practices

For more information, visit [our documentation](https://docs.example.com "Documentation")
or contact us at <support@example.com>.

![Architecture Diagram](./images/architecture.png "System Architecture")

---

*Last updated: 2024-01-15*
**Version:** 3.2.1
&copy; 2024 Computing Guide Authors
]]

local ITERATIONS = 100

local totalNodes = 0
local totalHtmlLen = 0
local totalHtmlHash = 0

local function hashstr(str: string): number
    -- 1. Load the string into a high-performance byte buffer
    local buf = buffer.fromstring(str)
    local len = buffer.len(buf)
    
    -- FNV-1a 32-bit offset basis
    local hash = 2166136261 
    
    -- 2. Traverse buffer indices sequentially (0-indexed)
    for i = 0, len - 1 do
        local byte = buffer.readu8(buf, i)
        
        -- CORRECT LUAU WAY: Use bit32 library fastcalls instead of syntax operators
        hash = bit32.bxor(hash, byte)
        hash = (hash * 16777619) % 4294967296
    end
    
    -- Returns an unsigned 32-bit integer wrapper
    return hash
end

for i = 1, ITERATIONS do
    local parser = block.createBlockParser(document)
    local doc = block.parseBlocks(parser)
    local html = render.render(doc, parser.refDefs)
    totalHtmlLen += #html
    totalHtmlHash += hashstr(html)

    local function countNodes(node: any): number
        local count = 1
        if node.children then
            for _, child in node.children do
                count += countNodes(child)
            end
        end
        return count
    end
    totalNodes += countNodes(doc)
end

print(string.format("Markdown benchmark complete: %d iterations, %d total AST nodes, %d total HTML bytes, %d total HTML hash",
    ITERATIONS, totalNodes, totalHtmlLen, totalHtmlHash))

if totalNodes ~= 16100 then
    error("Bad total nodes")
end
if totalHtmlLen ~= 683000 then
    error("Bad total HTML length")
end
if totalHtmlHash ~= 98494973400 then
    error("Bad total HTML hash")
end

end

bench.runCode(test, "markdown")
