// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
// This code is based on Lua 5.x implementation licensed under MIT License; see lua_LICENSE.txt for details
#pragma once

// When debugging complex issues, consider enabling one of these:
// This will reallocate the stack very aggressively at every opportunity; use this with asan to catch stale stack pointers
// #define HARDSTACKTESTS 1
// This will call GC validation very aggressively at every incremental GC step; use this with caution as it's SLOW
// #define HARDMEMTESTS 1
// This will call GC validation very aggressively at every GC opportunity; use this with caution as it's VERY SLOW
// #define HARDMEMTESTS 2

// To force MSVC2017+ to generate SSE2 code for some stdlib functions we need to locally enable /fp:fast
// Note that /fp:fast changes the semantics of floating point comparisons so this is only safe to do for functions without ones
#if defined(_MSC_VER) && !defined(__clang__)
#define LUAU_FASTMATH_BEGIN __pragma(float_control(precise, off, push))
#define LUAU_FASTMATH_END __pragma(float_control(pop))
#else
#define LUAU_FASTMATH_BEGIN
#define LUAU_FASTMATH_END
#endif

// Used on functions that have a printf-like interface to validate them statically
#if defined(__GNUC__)
#define LUA_PRINTF_ATTR(fmt, arg) __attribute__((format(printf, fmt, arg)))
#else
#define LUA_PRINTF_ATTR(fmt, arg)
#endif

#ifdef _MSC_VER
#define LUA_NORETURN __declspec(noreturn)
#else
#define LUA_NORETURN __attribute__((__noreturn__))
#endif

/* Can be used to reconfigure visibility/exports for public APIs */
#ifndef LUA_API
#define LUA_API extern
#endif

#define LUALIB_API LUA_API

/* Can be used to reconfigure visibility for internal APIs */
#if defined(__GNUC__)
#define LUAI_FUNC __attribute__((visibility("hidden"))) extern
#define LUAI_DATA LUAI_FUNC
#else
#define LUAI_FUNC extern
#define LUAI_DATA extern
#endif

/* Can be used to reconfigure internal error handling to use longjmp instead of C++ EH */
#ifndef LUA_USE_LONGJMP
#define LUA_USE_LONGJMP 0
#endif

/* LUA_IDSIZE gives the maximum size for the description of the source */
#ifndef LUA_IDSIZE
#define LUA_IDSIZE 256
#endif

/*
@@ LUAI_GCGOAL defines the desired top heap size in relation to the live heap
@* size at the end of the GC cycle
** CHANGE it if you want the GC to run faster or slower (higher values
** mean larger GC pauses which mean slower collection.) You can also change
** this value dynamically.
*/
#ifndef LUAI_GCGOAL
#define LUAI_GCGOAL 200 /* 200% (allow heap to double compared to live heap size) */
#endif

/*
@@ LUAI_GCSTEPMUL / LUAI_GCSTEPSIZE define the default speed of garbage collection
@* relative to memory allocation.
** Every LUAI_GCSTEPSIZE KB allocated, incremental collector collects LUAI_GCSTEPSIZE
** times LUAI_GCSTEPMUL% bytes.
** CHANGE it if you want to change the granularity of the garbage
** collection.
*/
#ifndef LUAI_GCSTEPMUL
#define LUAI_GCSTEPMUL 200 /* GC runs 'twice the speed' of memory allocation */
#endif

#ifndef LUAI_GCSTEPSIZE
#define LUAI_GCSTEPSIZE 1  /* GC runs every KB of memory allocation */
#endif

/* LUA_MINSTACK is the guaranteed number of Lua stack slots available to a C function */
#ifndef LUA_MINSTACK
#define LUA_MINSTACK 20
#endif

/* LUAI_MAXCSTACK limits the number of Lua stack slots that a C function can use */
#ifndef LUAI_MAXCSTACK
#define LUAI_MAXCSTACK 8000
#endif

/* LUAI_MAXCALLS limits the number of nested calls */
#ifndef LUAI_MAXCALLS
#define LUAI_MAXCALLS 20000
#endif

/* LUAI_MAXCCALLS is the maximum depth for nested C calls; this limit depends on native stack size */
#ifndef LUAI_MAXCCALLS
#define LUAI_MAXCCALLS 200
#endif

/* buffer size used for on-stack string operations; this limit depends on native stack size */
#ifndef LUA_BUFFERSIZE
#define LUA_BUFFERSIZE 512
#endif

/* number of valid Lua userdata tags */
#ifndef LUA_UTAG_LIMIT
#define LUA_UTAG_LIMIT 128
#endif

/* upper bound for number of size classes used by page allocator */
#ifndef LUA_SIZECLASSES
#define LUA_SIZECLASSES 32
#endif

/* available number of separate memory categories */
#ifndef LUA_MEMORY_CATEGORIES
#define LUA_MEMORY_CATEGORIES 256
#endif

/* minimum size for the string table (must be power of 2) */
#ifndef LUA_MINSTRTABSIZE
#define LUA_MINSTRTABSIZE 32
#endif

/* maximum number of captures supported by pattern matching */
#ifndef LUA_MAXCAPTURES
#define LUA_MAXCAPTURES 32
#endif

/* }================================================================== */

/*
@@ LUAI_USER_ALIGNMENT_T is a type that requires maximum alignment.
** CHANGE it if your system requires alignments larger than double. (For
** instance, if your system supports long doubles and they must be
** aligned in 16-byte boundaries, then you should add long double in the
** union.) Probably you do not need to change this.
*/
#define LUAI_USER_ALIGNMENT_T \
    union \
    { \
        double u; \
        void* s; \
        long l; \
    }

#define LUA_VECTOR_SIZE 3	/* must be 3 or 4 */

#define LUA_EXTRA_SIZE LUA_VECTOR_SIZE - 2
