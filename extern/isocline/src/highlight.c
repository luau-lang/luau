/* ----------------------------------------------------------------------------
  Copyright (c) 2021, Daan Leijen
  This is free software; you can redistribute it and/or modify it
  under the terms of the MIT License. A copy of the license can be
  found in the "LICENSE" file at the root of this distribution.
-----------------------------------------------------------------------------*/

#include <string.h>
#include "common.h"
#include "term.h"
#include "stringbuf.h"
#include "attr.h"
#include "bbcode.h"

//-------------------------------------------------------------
// Syntax highlighting
//-------------------------------------------------------------

struct ic_highlight_env_s {
  attrbuf_t*    attrs;
  const char*   input;   
  ssize_t       input_len;     
  bbcode_t*     bbcode;
  alloc_t*      mem;
  ssize_t       cached_upos;  // cached unicode position
  ssize_t       cached_cpos;  // corresponding utf-8 byte position
};


ic_private void highlight( alloc_t* mem, bbcode_t* bb, const char* s, attrbuf_t* attrs, ic_highlight_fun_t* highlighter, void* arg ) {
  const ssize_t len = ic_strlen(s);
  if (len <= 0) return;
  attrbuf_set_at(attrs,0,len,attr_none()); // fill to length of s
  if (highlighter != NULL) {
    ic_highlight_env_t henv;
    henv.attrs = attrs;
    henv.input = s;     
    henv.input_len = len;
    henv.bbcode = bb;
    henv.mem = mem;
    henv.cached_cpos = 0;
    henv.cached_upos = 0;
    (*highlighter)( &henv, s, arg );    
  }
}


//-------------------------------------------------------------
// Client interface
//-------------------------------------------------------------

static void pos_adjust( ic_highlight_env_t* henv, ssize_t* ppos, ssize_t* plen ) {
  ssize_t pos = *ppos;
  ssize_t len = *plen;
  if (pos >= henv->input_len) return;
  if (pos >= 0 && len >= 0) return;   // already character positions
  if (henv->input == NULL) return;

  if (pos < 0) {
    // negative `pos` is used as the unicode character position (for easy interfacing with Haskell)
    ssize_t upos = -pos;
    ssize_t cpos = 0;
    ssize_t ucount = 0;
    if (henv->cached_upos <= upos) {  // if we have a cached position, start from there
      ucount = henv->cached_upos;
      cpos = henv->cached_cpos;
    }
    while ( ucount < upos ) {
      ssize_t next = str_next_ofs(henv->input, henv->input_len, cpos, NULL);
      if (next <= 0) return;
      ucount++;
      cpos += next;
    }
    *ppos = pos = cpos;
    // and cache it to avoid quadratic behavior
    henv->cached_upos = upos;
    henv->cached_cpos = cpos;
  }
  if (len < 0) {
    // negative `len` is used as a unicode character length
    len = -len;
    ssize_t ucount = 0;
    ssize_t clen   = 0;    
    while (ucount < len) {
      ssize_t next = str_next_ofs(henv->input, henv->input_len, pos + clen, NULL);
      if (next <= 0) return;
      ucount++;
      clen += next;
    }
    *plen = len = clen;
    // and update cache if possible
    if (henv->cached_cpos == pos) {
      henv->cached_upos += ucount;
      henv->cached_cpos += clen;
    }
  } 
}

static void highlight_attr(ic_highlight_env_t* henv, ssize_t pos, ssize_t count, attr_t attr ) {
  if (henv==NULL) return;
  pos_adjust(henv,&pos,&count);
  if (pos < 0 || count <= 0) return;
  attrbuf_update_at(henv->attrs, pos, count, attr);
}

ic_public void ic_highlight(ic_highlight_env_t* henv, long pos, long count, const char* style ) {
  if (henv == NULL || style==NULL || style[0]==0 || pos < 0) return;  
  highlight_attr(henv,pos,count,bbcode_style( henv->bbcode, style ));
}

ic_public void ic_highlight_formatted(ic_highlight_env_t* henv, const char* s, const char* fmt) {
  if (s==NULL || s[0] == 0 || fmt==NULL) return;
  attrbuf_t* attrs = attrbuf_new(henv->mem);
  stringbuf_t* out = sbuf_new(henv->mem);  // todo: avoid allocating out?
  if (attrs!=NULL && out != NULL) {
    bbcode_append( henv->bbcode, fmt, out, attrs);
    const ssize_t len = ic_strlen(s);
    if (sbuf_len(out) != len) {
      debug_msg("highlight: formatted string content differs from the original input:\n  original: %s\n  formatted: %s\n", s, fmt);
    }
    for( ssize_t i = 0; i < len; i++) {
      attrbuf_update_at(henv->attrs, i, 1, attrbuf_attr_at(attrs,i));
    }
  }
  sbuf_free(out);
  attrbuf_free(attrs);
}

//-------------------------------------------------------------
// Brace matching
//-------------------------------------------------------------
#define MAX_NESTING (64)

typedef struct brace_s {
  char    close;
  bool    at_cursor;
  ssize_t pos;
} brace_t;

ic_private void highlight_match_braces(const char* s, attrbuf_t* attrs, ssize_t cursor_pos, const char* braces, attr_t match_attr, attr_t error_attr) 
{
  brace_t open[MAX_NESTING+1];
  ssize_t nesting = 0;
  const ssize_t brace_len = ic_strlen(braces);
  for (long i = 0; i < ic_strlen(s); i++) {
    const char c = s[i];
    // push open brace
    bool found_open = false;
    for (ssize_t b = 0; b < brace_len; b += 2) {
      if (c == braces[b]) {
        // open brace
        if (nesting >= MAX_NESTING) return; // give up
        open[nesting].close = braces[b+1];
        open[nesting].pos = i;
        open[nesting].at_cursor = (i == cursor_pos - 1);
        nesting++;
        found_open = true;
        break;
      }
    }
    if (found_open) continue;

    // pop to closing brace and potentially highlight
    for (ssize_t b = 1; b < brace_len; b += 2) {
      if (c == braces[b]) {
        // close brace
        if (nesting <= 0) {
          // unmatched close brace
          attrbuf_update_at( attrs, i, 1, error_attr);
        }
        else {
          // can we fix an unmatched brace where we can match by popping just one?
          if (open[nesting-1].close != c && nesting > 1 && open[nesting-2].close == c) {
            // assume previous open brace was wrong
            attrbuf_update_at(attrs, open[nesting-1].pos, 1, error_attr);
            nesting--;
          }
          if (open[nesting-1].close != c) {
            // unmatched open brace
            attrbuf_update_at( attrs, i, 1, error_attr);
          }
          else {
            // matching brace
            nesting--;
            if (i == cursor_pos - 1 || (open[nesting].at_cursor && open[nesting].pos != i - 1)) {
              // highlight matching brace
              attrbuf_update_at(attrs, open[nesting].pos, 1, match_attr);
              attrbuf_update_at(attrs, i, 1, match_attr);              
            }
          }
        }
        break;
      }
    }
  }
  // note: don't mark further unmatched open braces as in error
}


ic_private ssize_t find_matching_brace(const char* s, ssize_t cursor_pos, const char* braces, bool* is_balanced) 
{
  if (is_balanced != NULL) { *is_balanced = false; }
  bool balanced = true;
  ssize_t match = -1;
  brace_t open[MAX_NESTING+1];
  ssize_t nesting = 0;
  const ssize_t brace_len = ic_strlen(braces);
  for (long i = 0; i < ic_strlen(s); i++) {
    const char c = s[i];
    // push open brace
    bool found_open = false;
    for (ssize_t b = 0; b < brace_len; b += 2) {
      if (c == braces[b]) {
        // open brace
        if (nesting >= MAX_NESTING) return -1; // give up
        open[nesting].close = braces[b+1];
        open[nesting].pos = i;
        open[nesting].at_cursor = (i == cursor_pos - 1);
        nesting++;
        found_open = true;
        break;
      }
    }
    if (found_open) continue;

    // pop to closing brace 
    for (ssize_t b = 1; b < brace_len; b += 2) {
      if (c == braces[b]) {
        // close brace
        if (nesting <= 0) {
          // unmatched close brace
          balanced = false;          
        }
        else {
          if (open[nesting-1].close != c) {
            // unmatched open brace
            balanced = false;
          }
          else {
            // matching brace
            nesting--;
            if (i == cursor_pos - 1) {
              // found matching open brace
              match = open[nesting].pos + 1;
            }
            else if (open[nesting].at_cursor) {
              // found matching close brace
              match = i + 1;
            }
          }
        }
        break; 
      }
    }
  }
  if (nesting != 0) { balanced = false; }
  if (is_balanced != NULL) { *is_balanced = balanced; }
  return match;
}
