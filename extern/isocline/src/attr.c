/* ----------------------------------------------------------------------------
  Copyright (c) 2021, Daan Leijen
  This is free software; you can redistribute it and/or modify it
  under the terms of the MIT License. A copy of the license can be
  found in the "LICENSE" file at the root of this distribution.
-----------------------------------------------------------------------------*/
#include <string.h>

#include "common.h"
#include "stringbuf.h" // str_next_ofs
#include "attr.h"
#include "term.h"      // color_from_ansi256

//-------------------------------------------------------------
// Attributes
//-------------------------------------------------------------

ic_private attr_t attr_none(void) {
  attr_t attr;
  attr.value = 0;
  return attr;
}

ic_private attr_t attr_default(void) {
  attr_t attr = attr_none();
  attr.x.color = IC_ANSI_DEFAULT;
  attr.x.bgcolor = IC_ANSI_DEFAULT;
  attr.x.bold = IC_OFF;
  attr.x.underline = IC_OFF; 
  attr.x.reverse = IC_OFF;
  attr.x.italic = IC_OFF; 
  return attr;
}

ic_private bool attr_is_none(attr_t attr) {
  return (attr.value == 0);
}

ic_private bool attr_is_eq(attr_t attr1, attr_t attr2) {
  return (attr1.value == attr2.value);
}

ic_private attr_t attr_from_color( ic_color_t color ) {
  attr_t attr = attr_none();
  attr.x.color = color;
  return attr;
}


ic_private attr_t attr_update_with( attr_t oldattr, attr_t newattr ) {
  attr_t attr = oldattr;
  if (newattr.x.color != IC_COLOR_NONE)   { attr.x.color = newattr.x.color; }
  if (newattr.x.bgcolor != IC_COLOR_NONE) { attr.x.bgcolor = newattr.x.bgcolor; }
  if (newattr.x.bold != IC_NONE)          { attr.x.bold = newattr.x.bold; }
  if (newattr.x.italic != IC_NONE)        { attr.x.italic = newattr.x.italic; }
  if (newattr.x.reverse != IC_NONE)       { attr.x.reverse = newattr.x.reverse; }
  if (newattr.x.underline != IC_NONE)     { attr.x.underline = newattr.x.underline; }
  return attr;
}

static bool sgr_is_digit(char c) {
  return (c >= '0' && c <= '9');
}

static bool sgr_is_sep( char c ) {
  return (c==';' || c==':');
}

static bool sgr_next_par(const char* s, ssize_t* pi, ssize_t* par) {
  const ssize_t i = *pi;
  ssize_t n = 0;
  while( sgr_is_digit(s[i+n])) { 
    n++; 
  }
  if (n==0) { 
    *par = 0;
    return true;
  }
  else {
    *pi = i+n;
    return ic_atoz(s+i, par);    
  }
}

static bool sgr_next_par3(const char* s, ssize_t* pi, ssize_t* p1, ssize_t* p2, ssize_t* p3) {
  bool ok = false;
  ssize_t i = *pi;
  if (sgr_next_par(s,&i,p1) && sgr_is_sep(s[i])) {
    i++;
    if (sgr_next_par(s,&i,p2) && sgr_is_sep(s[i])) {
      i++;
      if (sgr_next_par(s,&i,p3)) {
        ok = true;
      };
    }
  }
  *pi = i;
  return ok;
}

ic_private attr_t attr_from_sgr( const char* s, ssize_t len) {
  attr_t attr = attr_none();
  for( ssize_t i = 0; i < len && s[i] != 0; i++) {
    ssize_t cmd = 0;
    if (!sgr_next_par(s,&i,&cmd)) continue;
    switch(cmd) {
      case  0: attr = attr_default(); break;
      case  1: attr.x.bold = IC_ON; break;
      case  3: attr.x.italic = IC_ON; break;
      case  4: attr.x.underline = IC_ON; break;
      case  7: attr.x.reverse = IC_ON; break;
      case 22: attr.x.bold = IC_OFF; break;
      case 23: attr.x.italic = IC_OFF; break;
      case 24: attr.x.underline = IC_OFF; break;
      case 27: attr.x.reverse = IC_OFF; break;
      case 39: attr.x.color = IC_ANSI_DEFAULT; break;
      case 49: attr.x.bgcolor = IC_ANSI_DEFAULT; break;
      default: {
        if (cmd >= 30 && cmd <= 37) {
          attr.x.color = IC_ANSI_BLACK + (unsigned)(cmd - 30);
        }
        else if (cmd >= 40 && cmd <= 47) {
          attr.x.bgcolor = IC_ANSI_BLACK + (unsigned)(cmd - 40);
        }
        else if (cmd >= 90 && cmd <= 97) {
          attr.x.color = IC_ANSI_DARKGRAY + (unsigned)(cmd - 90);
        }
        else if (cmd >= 100 && cmd <= 107) {
          attr.x.bgcolor = IC_ANSI_DARKGRAY + (unsigned)(cmd - 100);
        }
        else if ((cmd == 38 || cmd == 48) && sgr_is_sep(s[i])) {
          // non-associative SGR :-(          
          ssize_t par = 0;
          i++;
          if (sgr_next_par(s, &i, &par)) {
            if (par==5 && sgr_is_sep(s[i])) {
              // ansi 256 index
              i++;
              if (sgr_next_par(s, &i, &par) && par >= 0 && par <= 0xFF) {
                ic_color_t color = color_from_ansi256(par);
                if (cmd==38) { attr.x.color = color; }
                        else { attr.x.bgcolor = color; }
              }
            }
            else if (par == 2 && sgr_is_sep(s[i])) {
              // rgb value
              i++;
              ssize_t r,g,b;
              if (sgr_next_par3(s, &i, &r,&g,&b)) {
                ic_color_t color = ic_rgbx(r,g,b);
                if (cmd==38) { attr.x.color = color; }
                        else { attr.x.bgcolor = color; }
              }
            }
          }
        }
        else {
          debug_msg("attr: unknow ANSI SGR code: %zd\n", cmd );
        }
      }
    }
  }
  return attr;
}

ic_private attr_t attr_from_esc_sgr( const char* s, ssize_t len) {
  if (len <= 2 || s[0] != '\x1B' || s[1] != '[' || s[len-1] != 'm') return attr_none();
  return attr_from_sgr(s+2, len-2);
}


//-------------------------------------------------------------
// Attribute buffer
//-------------------------------------------------------------
struct attrbuf_s {
  attr_t*  attrs;
  ssize_t  capacity;
  ssize_t  count;
  alloc_t* mem;
};

static bool attrbuf_ensure_capacity( attrbuf_t* ab, ssize_t needed ) {
  if (needed <= ab->capacity) return true;
  ssize_t newcap = (ab->capacity <= 0 ? 240 : (ab->capacity > 1000 ? ab->capacity + 1000 : 2*ab->capacity));
  if (needed > newcap) { newcap = needed; }
  attr_t* newattrs = mem_realloc_tp( ab->mem, attr_t, ab->attrs, newcap );
  if (newattrs == NULL) return false;
  ab->attrs = newattrs;
  ab->capacity = newcap;
  assert(needed <= ab->capacity);
  return true;
}

static bool attrbuf_ensure_extra( attrbuf_t* ab, ssize_t extra ) {
  const ssize_t needed = ab->count + extra;
  return attrbuf_ensure_capacity( ab, needed );
}


ic_private attrbuf_t* attrbuf_new( alloc_t* mem ) {
  attrbuf_t* ab = mem_zalloc_tp(mem,attrbuf_t);
  if (ab == NULL) return NULL;
  ab->mem = mem;
  attrbuf_ensure_extra(ab,1);
  return ab;
}

ic_private void attrbuf_free( attrbuf_t* ab ) {
  if (ab==NULL) return;
  mem_free(ab->mem, ab->attrs);
  mem_free(ab->mem, ab);
}

ic_private void attrbuf_clear(attrbuf_t* ab) {
  if (ab == NULL) return;
  ab->count = 0;
}

ic_private ssize_t attrbuf_len( attrbuf_t* ab ) {
  return (ab==NULL ? 0 : ab->count);
}

ic_private const attr_t* attrbuf_attrs( attrbuf_t* ab, ssize_t expected_len ) {
  assert(expected_len <= ab->count );
  // expand if needed
  if (ab->count < expected_len) {    
    if (!attrbuf_ensure_capacity(ab,expected_len)) return NULL;
    for(ssize_t i = ab->count; i < expected_len; i++) {
      ab->attrs[i] = attr_none();  
    }
    ab->count = expected_len;
  }
  return ab->attrs;
}



static void attrbuf_update_set_at( attrbuf_t* ab, ssize_t pos, ssize_t count, attr_t attr, bool update ) {
  const ssize_t end = pos + count;
  if (!attrbuf_ensure_capacity(ab, end)) return;
  ssize_t i;
  // initialize if end is beyond the count (todo: avoid duplicate init and set if update==false?)
  if (ab->count < end) {
    for(i = ab->count; i < end; i++) {
      ab->attrs[i] = attr_none();  
    }
    ab->count = end;
  }
  // fill pos to end with attr 
  for(i = pos; i < end; i++) {
    ab->attrs[i] = (update ? attr_update_with(ab->attrs[i],attr) : attr);    
  }  
}

ic_private void attrbuf_set_at( attrbuf_t* ab, ssize_t pos, ssize_t count, attr_t attr ) {
  attrbuf_update_set_at(ab, pos, count, attr, false);
}

ic_private void attrbuf_update_at( attrbuf_t* ab, ssize_t pos, ssize_t count, attr_t attr ) {
  attrbuf_update_set_at(ab, pos, count, attr, true);  
}

ic_private void attrbuf_insert_at( attrbuf_t* ab, ssize_t pos, ssize_t count, attr_t attr ) {
  if (pos < 0 || pos > ab->count || count <= 0) return;
  if (!attrbuf_ensure_extra(ab,count)) return;  
  ic_memmove( ab->attrs + pos + count, ab->attrs + pos, (ab->count - pos)*ssizeof(attr_t) );
  ab->count += count;
  attrbuf_set_at( ab, pos, count, attr );
}


// note: must allow ab == NULL!
ic_private ssize_t attrbuf_append_n( stringbuf_t* sb, attrbuf_t* ab, const char* s, ssize_t len, attr_t attr ) {
  if (s == NULL || len == 0) return sbuf_len(sb);
  if (ab != NULL) {
    if (!attrbuf_ensure_extra(ab,len)) return sbuf_len(sb);
    attrbuf_set_at(ab, ab->count, len, attr);
  }
  return sbuf_append_n(sb,s,len);
}

ic_private attr_t attrbuf_attr_at( attrbuf_t* ab, ssize_t pos ) {
  if (ab==NULL || pos < 0 || pos > ab->count) return attr_none();
  return ab->attrs[pos];
}

ic_private void attrbuf_delete_at( attrbuf_t* ab, ssize_t pos, ssize_t count ) {
  if (ab==NULL || pos < 0 || pos > ab->count) return;
  if (pos + count > ab->count) { count = ab->count - pos; }
  if (count == 0) return;
  assert(pos + count <= ab->count);
  ic_memmove( ab->attrs + pos, ab->attrs + pos + count, ab->count - (pos + count) );
  ab->count -= count;
}
