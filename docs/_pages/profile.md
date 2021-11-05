---
permalink: /profile
title: Profiling
toc: true
---

One of main goals of Luau is to enable high performance code. To help with that goal, we are relentlessly optimizing the compiler and runtime - but ultimately, performance of their
code is in developers' hands, and is a combination of good algorithm design and implementation that adheres to the strengths of the language. To help write efficient code, Luau
provides a built-in profiler that samples the execution of the program and outputs a profiler dump that can be converted to an interactive flamegraph.

To run the profiler, make sure you have an optimized build of the intepreter (otherwise profiling results are going to be very skewed) and run it with `--profile` argument:

```
$ luau --profile tests/chess.lua
OK      8902    rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1
OK      2039    r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 0
OK      2812    8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 0
OK      9467    r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0 1
OK      1486    rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8
OK      2079    r4rk1/1pp1qppp/p1np1n2/2b1p1B1/2B1P1b1/P1NP1N2/1PP1QPPP/R4RK1 w - - 0 10
Profiler dump written to profile.out (total runtime 2.034 seconds, 20344 samples, 374 stacks)
GC: 0.378 seconds (18.58%), mark 46.80%, remark 3.33%, atomic 1.93%, sweepstring 6.77%, sweep 41.16%
```

The resulting `profile.out` file can be converted to an SVG file by running `perfgraph.py` script that is part of Luau repository:

```
$ python tools/perfgraph.py profile.out >profile.svg
```

This produces an SVG file that can be opened in a browser (the image below is clickable):

[![profile.svg](/assets/images/chess-profile.svg)](/assets/images/chess-profile.svg)

In a flame graph visualization, the individual bars represent function calls, the width represents how much of the total program runtime they execute, and the nesting matches the call stack encountered during program execution. This is a fantastic visualization technique that allows you to hone in on the specific bottlenecks affecting
your program performance, optimize those exact bottlenecks, and then re-generate the profile data and visualizer, and look for the next set of true bottlenecks (if any).

Hovering your mouse cursor over individual sections will display detailed function information in the status bar and in a tooltip. If you want to Search for a specific named
function, use the Search field in the upper right, or press Ctrl+F.

Notice that some of the bars in the screenshot don't have any text. In some cases, there isn't enough room in the size of the bar to display the name.
You can hover your mouse over those bars to see the name and source location of the function in the tool tip, or double-click to zoom in on that part of the flame graph.

Some tooltips will have a source location for the function you're hovering over, but no name. Those are anonymous functions, or functions that were not declared in a way that
allows Luau compiler to track the name. To fill in more names, you may want to make these changes to your code:

`local myFunc = function() --[[ work ]] end` -> `local function myFunc() --[[ work ]] end`

Even without these changes, you can hover over a given bar with no visible name and see it's source location. 

As any sampling profiler, this profiler relies on gathering enough information for the resulting output to be statistically meaningful. It may miss short functions if they
aren't called often enough. By default the profiler runs at 10 kHz, this can be customized by passing a different parameter to `--profile=`. Note that higher
frequencies result in higher profiling overhead and longer program execution, potentially skewing the results.

This profiler doesn't track leaf C functions and instead attributes the time spent there to calling Luau functions. As a result, when thinking about why a given function is
slow, consider not just the work it does immediately but also the library functions it calls.

This profiler tracks time consumed by Luau thread stacks; when a thread calls another thread via `coroutine.resume`, the time spent is not attributed to the parent thread that's
waiting for resume results. This limitation will be removed in the future in the future.
