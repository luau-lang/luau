/* ----------------------------------------------------------------------------
  Copyright (c) 2021, Daan Leijen
  This is free software; you can redistribute it and/or modify it
  under the terms of the MIT License. A copy of the license can be
  found in the "LICENSE" file at the root of this distribution.
-----------------------------------------------------------------------------*/

#pragma once
#ifndef IC_COMMON_H
#define IC_COMMON_H

//-------------------------------------------------------------
// Headers and defines
//-------------------------------------------------------------

#include <sys/types.h>  // ssize_t
#include <limits.h>
#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>
#include <assert.h>
#include "../include/isocline.h"  // ic_malloc_fun_t, ic_color_t etc.

# ifdef __cplusplus
#  define ic_extern_c   extern "C"
# else
#  define ic_extern_c
# endif

#if defined(IC_SEPARATE_OBJS)
#  define ic_public     ic_extern_c 
# if defined(__GNUC__) // includes clang and icc      
#  define ic_private    __attribute__((visibility("hidden")))
# else
#  define ic_private  
# endif
#else
# define ic_private     static
# define ic_public      ic_extern_c
#endif

#define ic_unused(x)    (void)(x)


//-------------------------------------------------------------
// ssize_t
//-------------------------------------------------------------

#if defined(_MSC_VER)
typedef intptr_t ssize_t;
#endif

#define ssizeof(tp)   (ssize_t)(sizeof(tp))
static inline size_t  to_size_t(ssize_t sz) { return (sz >= 0 ? (size_t)sz : 0); }
static inline ssize_t to_ssize_t(size_t sz) { return (sz <= SIZE_MAX/2 ? (ssize_t)sz : 0); }

ic_private void    ic_memmove(void* dest, const void* src, ssize_t n);
ic_private void    ic_memcpy(void* dest, const void* src, ssize_t n);
ic_private void    ic_memset(void* dest, uint8_t value, ssize_t n);
ic_private bool    ic_memnmove(void* dest, ssize_t dest_size, const void* src, ssize_t n);

ic_private ssize_t ic_strlen(const char* s);
ic_private bool    ic_strcpy(char* dest, ssize_t dest_size /* including 0 */, const char* src);
ic_private bool    ic_strncpy(char* dest, ssize_t dest_size /* including 0 */, const char* src, ssize_t n);

ic_private bool    ic_contains(const char* big, const char* s);
ic_private bool    ic_icontains(const char* big, const char* s);
ic_private char    ic_tolower(char c);
ic_private void    ic_str_tolower(char* s);
ic_private int     ic_stricmp(const char* s1, const char* s2);
ic_private int     ic_strnicmp(const char* s1, const char* s2, ssize_t n);



//---------------------------------------------------------------------
// Unicode
//
// We use "qutf-8" (quite like utf-8) encoding and decoding. 
// Internally we always use valid utf-8. If we encounter invalid
// utf-8 bytes (or bytes >= 0x80 from any other encoding) we encode
// these as special code points in the "raw plane" (0xEE000 - 0xEE0FF).
// When decoding we are then able to restore such raw bytes as-is.
// See <https://github.com/koka-lang/koka/blob/master/kklib/include/kklib/string.h>
//---------------------------------------------------------------------

typedef uint32_t  unicode_t;

ic_private void      unicode_to_qutf8(unicode_t u, uint8_t buf[5]);
ic_private unicode_t unicode_from_qutf8(const uint8_t* s, ssize_t len, ssize_t* nread); // validating

ic_private unicode_t unicode_from_raw(uint8_t c);
ic_private bool      unicode_is_raw(unicode_t u, uint8_t* c);

ic_private bool      utf8_is_cont(uint8_t c);


//-------------------------------------------------------------
// Colors
//-------------------------------------------------------------

// A color is either RGB or an ANSI code.
// (RGB colors have bit 24 set to distinguish them from the ANSI color palette colors.)
// (Isocline will automatically convert from RGB on terminals that do not support full colors)
typedef uint32_t ic_color_t;

// Create a color from a 24-bit color value.
ic_private ic_color_t ic_rgb(uint32_t hex);

// Create a color from a 8-bit red/green/blue components.
// The value of each component is capped between 0 and 255.
ic_private ic_color_t ic_rgbx(ssize_t r, ssize_t g, ssize_t b);

#define IC_COLOR_NONE     (0)
#define IC_RGB(rgb)       (0x1000000 | (uint32_t)(rgb)) // ic_rgb(rgb)  // define to it can be used as a constant

// ANSI colors.
// The actual colors used is usually determined by the terminal theme
// See <https://en.wikipedia.org/wiki/ANSI_escape_code#3-bit_and_4-bit>
#define IC_ANSI_BLACK     (30)
#define IC_ANSI_MAROON    (31)
#define IC_ANSI_GREEN     (32)
#define IC_ANSI_OLIVE     (33)
#define IC_ANSI_NAVY      (34)
#define IC_ANSI_PURPLE    (35)
#define IC_ANSI_TEAL      (36)
#define IC_ANSI_SILVER    (37)
#define IC_ANSI_DEFAULT   (39)

#define IC_ANSI_GRAY      (90)
#define IC_ANSI_RED       (91)
#define IC_ANSI_LIME      (92)
#define IC_ANSI_YELLOW    (93)
#define IC_ANSI_BLUE      (94)
#define IC_ANSI_FUCHSIA   (95)
#define IC_ANSI_AQUA      (96)
#define IC_ANSI_WHITE     (97)

#define IC_ANSI_DARKGRAY  IC_ANSI_GRAY
#define IC_ANSI_LIGHTGRAY IC_ANSI_SILVER
#define IC_ANSI_MAGENTA   IC_ANSI_FUCHSIA
#define IC_ANSI_CYAN      IC_ANSI_AQUA



//-------------------------------------------------------------
// Debug
//-------------------------------------------------------------

#if defined(IC_NO_DEBUG_MSG) 
#define debug_msg(fmt,...)   (void)(0)
#else
ic_private void debug_msg( const char* fmt, ... );
#endif


//-------------------------------------------------------------
// Abstract environment
//-------------------------------------------------------------
struct ic_env_s;
typedef struct ic_env_s ic_env_t;


//-------------------------------------------------------------
// Allocation
//-------------------------------------------------------------

typedef struct alloc_s {
  ic_malloc_fun_t*  malloc;
  ic_realloc_fun_t* realloc;
  ic_free_fun_t*    free;
} alloc_t;


ic_private void* mem_malloc( alloc_t* mem, ssize_t sz );
ic_private void* mem_zalloc( alloc_t* mem, ssize_t sz );
ic_private void* mem_realloc( alloc_t* mem, void* p, ssize_t newsz );
ic_private void  mem_free( alloc_t* mem, const void* p );
ic_private char* mem_strdup( alloc_t* mem, const char* s);
ic_private char* mem_strndup( alloc_t* mem, const char* s, ssize_t n);

#define mem_zalloc_tp(mem,tp)        (tp*)mem_zalloc(mem,ssizeof(tp))
#define mem_malloc_tp_n(mem,tp,n)    (tp*)mem_malloc(mem,(n)*ssizeof(tp))
#define mem_zalloc_tp_n(mem,tp,n)    (tp*)mem_zalloc(mem,(n)*ssizeof(tp))
#define mem_realloc_tp(mem,tp,p,n)   (tp*)mem_realloc(mem,p,(n)*ssizeof(tp))


#endif // IC_COMMON_H
