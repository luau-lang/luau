/* ----------------------------------------------------------------------------
  Copyright (c) 2021, Daan Leijen
  This is free software; you can redistribute it and/or modify it
  under the terms of the MIT License. A copy of the license can be
  found in the "LICENSE" file at the root of this distribution.
-----------------------------------------------------------------------------*/

#include <string.h>
#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include "common.h"


//-------------------------------------------------------------
// String wrappers for ssize_t
//-------------------------------------------------------------

ic_private ssize_t ic_strlen( const char* s ) {
  if (s==NULL) return 0;
  return to_ssize_t(strlen(s));
}

ic_private void ic_memmove( void* dest, const void* src, ssize_t n ) {
  assert(dest!=NULL && src != NULL);
  if (n <= 0) return;
  memmove(dest,src,to_size_t(n));
}


ic_private void ic_memcpy( void* dest, const void* src, ssize_t n ) {
  assert(dest!=NULL && src != NULL);
  if (dest == NULL || src == NULL || n <= 0) return;
  memcpy(dest,src,to_size_t(n));
}

ic_private void ic_memset(void* dest, uint8_t value, ssize_t n) {
  assert(dest!=NULL);
  if (dest == NULL || n <= 0) return;
  memset(dest,(int8_t)value,to_size_t(n));
}

ic_private bool ic_memnmove( void* dest, ssize_t dest_size, const void* src, ssize_t n ) {
  assert(dest!=NULL && src != NULL);
  if (n <= 0) return true;
  if (dest_size < n) { assert(false); return false; }
  memmove(dest,src,to_size_t(n));
  return true;
}

ic_private bool ic_strcpy( char* dest, ssize_t dest_size /* including 0 */, const char* src) {
  assert(dest!=NULL && src != NULL);
  if (dest == NULL || dest_size <= 0) return false;
  ssize_t slen = ic_strlen(src);
  if (slen >= dest_size) return false;
  strcpy(dest,src);
  assert(dest[slen] == 0);
  return true;
}


ic_private bool ic_strncpy( char* dest, ssize_t dest_size /* including 0 */, const char* src, ssize_t n) {
  assert(dest!=NULL && n < dest_size);
  if (dest == NULL || dest_size <= 0) return false;
  if (n >= dest_size) return false;
  if (src==NULL || n <= 0) {
    dest[0] = 0;
  }
  else {
    strncpy(dest,src,to_size_t(n));
    dest[n] = 0;  
  }
  return true;
}

//-------------------------------------------------------------
// String matching
//-------------------------------------------------------------

ic_public bool ic_starts_with( const char* s, const char* prefix ) {
  if (s==prefix) return true;
  if (prefix==NULL) return true;
  if (s==NULL) return false;

  ssize_t i;
  for( i = 0; s[i] != 0 && prefix[i] != 0; i++) {
    if (s[i] != prefix[i]) return false;
  }
  return (prefix[i] == 0);
}

ic_private char ic_tolower( char c ) {
  return (c >= 'A' && c <= 'Z'  ?  c - 'A' + 'a' : c);
}

ic_private void ic_str_tolower(char* s) {
  while(*s != 0) {
    *s = ic_tolower(*s);
    s++;
  }
}

ic_public bool ic_istarts_with( const char* s, const char* prefix ) {
  if (s==prefix) return true;
  if (prefix==NULL) return true;
  if (s==NULL) return false;

  ssize_t i;
  for( i = 0; s[i] != 0 && prefix[i] != 0; i++) {
    if (ic_tolower(s[i]) != ic_tolower(prefix[i])) return false;
  }
  return (prefix[i] == 0);
}


ic_private int ic_strnicmp(const char* s1, const char* s2, ssize_t n) {
  if (s1 == NULL && s2 == NULL) return 0;
  if (s1 == NULL) return -1;
  if (s2 == NULL) return 1;
  ssize_t i;
  for (i = 0; s1[i] != 0 && i < n; i++) {  // note: if s2[i] == 0 the loop will stop as c1 != c2
    char c1 = ic_tolower(s1[i]);
    char c2 = ic_tolower(s2[i]);
    if (c1 < c2) return -1;
    if (c1 > c2) return 1;
  }
  return ((i >= n || s2[i] == 0) ? 0 : -1);
}

ic_private int ic_stricmp(const char* s1, const char* s2) {
  ssize_t len1 = ic_strlen(s1);
  ssize_t len2 = ic_strlen(s2);
  if (len1 < len2) return -1;
  if (len1 > len2) return 1;
  return (ic_strnicmp(s1, s2, (len1 >= len2 ? len1 : len2)));
}


static const char* ic_stristr(const char* s, const char* pat) {
  if (s==NULL) return NULL;
  if (pat==NULL || pat[0] == 0) return s;
  ssize_t patlen = ic_strlen(pat);
  for (ssize_t i = 0; s[i] != 0; i++) {
    if (ic_strnicmp(s + i, pat, patlen) == 0) return (s+i);
  }
  return NULL;
}

ic_private bool ic_contains(const char* big, const char* s) {
  if (big == NULL) return false;
  if (s == NULL) return true;
  return (strstr(big,s) != NULL);
}

ic_private bool ic_icontains(const char* big, const char* s) {
  if (big == NULL) return false;
  if (s == NULL) return true;
  return (ic_stristr(big,s) != NULL);
}


//-------------------------------------------------------------
// Unicode
// QUTF-8: See <https://github.com/koka-lang/koka/blob/master/kklib/include/kklib/string.h>
// Raw bytes are code points 0xEE000 - 0xEE0FF
//-------------------------------------------------------------
#define IC_UNICODE_RAW   ((unicode_t)(0xEE000U))

ic_private unicode_t unicode_from_raw(uint8_t c) {
  return (IC_UNICODE_RAW + c);
}

ic_private bool unicode_is_raw(unicode_t u, uint8_t* c) {
  if (u >= IC_UNICODE_RAW && u <= IC_UNICODE_RAW + 0xFF) {
    *c = (uint8_t)(u - IC_UNICODE_RAW);
    return true;
  }
  else {
    return false;
  }
}

ic_private void unicode_to_qutf8(unicode_t u, uint8_t buf[5]) {
  memset(buf, 0, 5);
  if (u <= 0x7F) {
    buf[0] = (uint8_t)u;
  }
  else if (u <= 0x07FF) {
    buf[0] = (0xC0 | ((uint8_t)(u >> 6)));
    buf[1] = (0x80 | (((uint8_t)u) & 0x3F));
  }
  else if (u <= 0xFFFF) {
    buf[0] = (0xE0 |  ((uint8_t)(u >> 12)));
    buf[1] = (0x80 | (((uint8_t)(u >>  6)) & 0x3F));
    buf[2] = (0x80 | (((uint8_t)u) & 0x3F));
  }
  else if (u <= 0x10FFFF) {
    if (unicode_is_raw(u, &buf[0])) {
      buf[1] = 0;
    }
    else {
      buf[0] = (0xF0 |  ((uint8_t)(u >> 18)));
      buf[1] = (0x80 | (((uint8_t)(u >> 12)) & 0x3F));
      buf[2] = (0x80 | (((uint8_t)(u >>  6)) & 0x3F));
      buf[3] = (0x80 | (((uint8_t)u) & 0x3F));
    }
  }
}

// is this a utf8 continuation byte?
ic_private bool utf8_is_cont(uint8_t c) {
  return ((c & 0xC0) == 0x80);
}

ic_private unicode_t unicode_from_qutf8(const uint8_t* s, ssize_t len, ssize_t* count) {
  unicode_t c0 = 0;
  if (len <= 0 || s == NULL) {
    goto fail;
  }
  // 1 byte
  c0 = s[0];
  if (c0 <= 0x7F && len >= 1) {
    if (count != NULL) *count = 1;
    return c0; 
  }
  else if (c0 <= 0xC1) { // invalid continuation byte or invalid 0xC0, 0xC1
    goto fail;
  }
  // 2 bytes
  else if (c0 <= 0xDF && len >= 2 && utf8_is_cont(s[1])) { 
    if (count != NULL) *count = 2;
    return (((c0 & 0x1F) << 6) | (s[1] & 0x3F));
  }
  // 3 bytes: reject overlong and surrogate halves
  else if (len >= 3 && 
           ((c0 == 0xE0 && s[1] >= 0xA0 && s[1] <= 0xBF && utf8_is_cont(s[2])) ||
            (c0 >= 0xE1 && c0 <= 0xEC && utf8_is_cont(s[1]) && utf8_is_cont(s[2])) 
          ))
  {
    if (count != NULL) *count = 3;
    return (((c0 & 0x0F) << 12) | ((unicode_t)(s[1] & 0x3F) << 6) | (s[2] & 0x3F));
  }
  // 4 bytes: reject overlong
  else if (len >= 4 && 
           (((c0 == 0xF0 && s[1] >= 0x90 && s[1] <= 0xBF && utf8_is_cont(s[2]) && utf8_is_cont(s[3])) ||
            (c0 >= 0xF1 && c0 <= 0xF3 && utf8_is_cont(s[1]) && utf8_is_cont(s[2]) && utf8_is_cont(s[3])) ||
            (c0 == 0xF4 && s[1] >= 0x80 && s[1] <= 0x8F && utf8_is_cont(s[2]) && utf8_is_cont(s[3]))) 
          )) 
  {
    if (count != NULL) *count = 4;
    return (((c0 & 0x07) << 18) | ((unicode_t)(s[1] & 0x3F) << 12) | ((unicode_t)(s[2] & 0x3F) << 6) | (s[3] & 0x3F));
  }  
fail:
  if (count != NULL) *count = 1;
  return unicode_from_raw(s[0]);
}


//-------------------------------------------------------------
// Debug
//-------------------------------------------------------------

#if defined(IC_NO_DEBUG_MSG) 
// nothing
#elif !defined(IC_DEBUG_TO_FILE)
ic_private void debug_msg(const char* fmt, ...) {
  if (getenv("ISOCLINE_DEBUG")) {
    va_list args;
    va_start(args, fmt);
    vfprintf(stderr, fmt, args);
    va_end(args);
  }
}
#else
ic_private void debug_msg(const char* fmt, ...) {
  static int debug_init;
  static const char* debug_fname = "isocline.debug.txt";
  // initialize?
  if (debug_init==0) {
    debug_init = -1;
    const char* rdebug = getenv("ISOCLINE_DEBUG");
    if (rdebug!=NULL && strcmp(rdebug,"1") == 0) {
      FILE* fdbg = fopen(debug_fname, "w");
      if (fdbg!=NULL) {
        debug_init = 1;
        fclose(fdbg);
      }
    }
  }
  if (debug_init <= 0) return;

  // write debug messages
  FILE* fdbg = fopen(debug_fname, "a");
  if (fdbg==NULL) return;
  va_list args;
  va_start(args, fmt);
  vfprintf(fdbg, fmt, args);
  fclose(fdbg);
  va_end(args);
}
#endif


//-------------------------------------------------------------
// Allocation
//-------------------------------------------------------------

ic_private void* mem_malloc(alloc_t* mem, ssize_t sz) {
  return mem->malloc(to_size_t(sz));
}

ic_private void* mem_zalloc(alloc_t* mem, ssize_t sz) {
  void* p = mem_malloc(mem, sz);
  if (p != NULL) memset(p, 0, to_size_t(sz));
  return p;
}

ic_private void* mem_realloc(alloc_t* mem, void* p, ssize_t newsz) {
  return mem->realloc(p, to_size_t(newsz));
}

ic_private void mem_free(alloc_t* mem, const void* p) {
  mem->free((void*)p);
}

ic_private char* mem_strdup(alloc_t* mem, const char* s) {
  if (s==NULL) return NULL;
  ssize_t n = ic_strlen(s);
  char* p = mem_malloc_tp_n(mem, char, n+1);
  if (p == NULL) return NULL;
  ic_memcpy(p, s, n+1);
  return p;
}

ic_private char* mem_strndup(alloc_t* mem, const char* s, ssize_t n) {
  if (s==NULL || n < 0) return NULL;
  char* p = mem_malloc_tp_n(mem, char, n+1);
  if (p == NULL) return NULL;
  ssize_t i;
  for (i = 0; i < n && s[i] != 0; i++) {
    p[i] = s[i];
  }
  assert(i <= n);
  p[i] = 0;
  return p;
}

