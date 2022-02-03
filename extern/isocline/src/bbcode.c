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
#include "attr.h"
#include "term.h"
#include "bbcode.h" 

//-------------------------------------------------------------
// HTML color table
//-------------------------------------------------------------

#include "bbcode_colors.c"

//-------------------------------------------------------------
// Types
//-------------------------------------------------------------

typedef struct style_s {
  const char*  name;  // name of the style
  attr_t  attr;  // attribute to apply
} style_t;

typedef enum align_e {
  IC_ALIGN_LEFT,
  IC_ALIGN_CENTER,
  IC_ALIGN_RIGHT
} align_t;

typedef struct width_s {
  ssize_t w;     // > 0
  align_t align;
  bool    dots;  // "..."  (e.g. "sentence...")
  char    fill;  // " "    (e.g. "hello      ")
} width_t;

typedef struct tag_s {  
  const char* name;   // tag open name
  attr_t  attr;       // the saved attribute before applying the style
  width_t width;      // start sequence of at most "width" columns
  ssize_t pos;        // start position in the output (used for width restriction)
} tag_t;


static void tag_init(tag_t* tag) {
  memset(tag,0,sizeof(*tag));  
}

struct bbcode_s {
  tag_t*       tags;              // stack of tags; one entry for each open tag
  ssize_t      tags_capacity;
  ssize_t      tags_nesting;   
  style_t*     styles;            // list of used defined styles
  ssize_t      styles_capacity;
  ssize_t      styles_count;
  term_t*      term;              // terminal
  alloc_t*     mem;               // allocator
  // caches
  stringbuf_t*  out;              // print buffer
  attrbuf_t*    out_attrs;
  stringbuf_t*  vout;             // vprintf buffer 
};


//-------------------------------------------------------------
// Create, helpers
//-------------------------------------------------------------

ic_private bbcode_t* bbcode_new( alloc_t* mem, term_t* term ) {
  bbcode_t* bb = mem_zalloc_tp(mem,bbcode_t);
  if (bb==NULL) return NULL;
  bb->mem = mem;
  bb->term = term;
  bb->out = sbuf_new(mem);
  bb->out_attrs = attrbuf_new(mem);
  bb->vout = sbuf_new(mem);
  return bb;
}

ic_private void bbcode_free( bbcode_t* bb ) {
  for(ssize_t i = 0; i < bb->styles_count; i++) {
    mem_free(bb->mem, bb->styles[i].name);
  }
  mem_free(bb->mem, bb->tags);
  mem_free(bb->mem, bb->styles);
  sbuf_free(bb->vout);
  sbuf_free(bb->out);
  attrbuf_free(bb->out_attrs);
  mem_free(bb->mem, bb);
}

ic_private void bbcode_style_add( bbcode_t* bb, const char* style_name, attr_t attr ) {
  if (bb->styles_count >= bb->styles_capacity) {
    ssize_t newlen = bb->styles_capacity + 32;
    style_t* p = mem_realloc_tp( bb->mem, style_t, bb->styles, newlen );
    if (p == NULL) return;
    bb->styles = p;
    bb->styles_capacity = newlen;
  }
  assert(bb->styles_count < bb->styles_capacity);
  bb->styles[bb->styles_count].name = mem_strdup( bb->mem, style_name );
  bb->styles[bb->styles_count].attr = attr;
  bb->styles_count++;
}

static ssize_t bbcode_tag_push( bbcode_t* bb, const tag_t* tag ) {
  if (bb->tags_nesting >= bb->tags_capacity) {
    ssize_t newcap = bb->tags_capacity + 32;
    tag_t* p = mem_realloc_tp( bb->mem, tag_t, bb->tags, newcap );
    if (p == NULL) return -1;
    bb->tags = p;
    bb->tags_capacity = newcap;    
  }
  assert(bb->tags_nesting < bb->tags_capacity);
  bb->tags[bb->tags_nesting] = *tag;
  bb->tags_nesting++;
  return (bb->tags_nesting-1);
}

static void bbcode_tag_pop( bbcode_t* bb, tag_t* tag ) {
  if (bb->tags_nesting <= 0) {
    if (tag != NULL) { tag_init(tag); }
  }
  else {
    bb->tags_nesting--;
    if (tag != NULL) {
      *tag = bb->tags[bb->tags_nesting];
    }    
  }
}

//-------------------------------------------------------------
// Invalid parse/values/balance
//-------------------------------------------------------------

static void bbcode_invalid(const char* fmt, ... ) {
  if (getenv("ISOCLINE_BBCODE_DEBUG") != NULL) {
    va_list args;
    va_start(args,fmt);
    vfprintf(stderr,fmt,args);
    va_end(args);
  }
}

//-------------------------------------------------------------
// Set attributes
//-------------------------------------------------------------


static attr_t bbcode_open( bbcode_t* bb, ssize_t out_pos, const tag_t* tag, attr_t current ) { 
  // save current and set
  tag_t cur;
  tag_init(&cur);
  cur.name  = tag->name;
  cur.attr  = current;  
  cur.width = tag->width;
  cur.pos   = out_pos;
  bbcode_tag_push(bb,&cur);
  return attr_update_with( current, tag->attr );
}

static bool bbcode_close( bbcode_t* bb, ssize_t base, const char* name, tag_t* pprev ) {
  // pop until match
  while (bb->tags_nesting > base) {
    tag_t prev;
    bbcode_tag_pop(bb,&prev);
    if (name==NULL || prev.name==NULL || ic_stricmp(prev.name,name) == 0) {
      // matched
      if (pprev != NULL) { *pprev = prev; }
      return true;
    }
    else {
      // unbalanced: we either continue popping or restore the tags depending if there is a matching open tag in our tags.
      bool has_open_tag = false;
      if (name != NULL) {
        for( ssize_t i = bb->tags_nesting - 1; i > base; i--) {
          if (bb->tags[i].name != NULL && ic_stricmp(bb->tags[i].name, name) == 0) {
            has_open_tag = true;
            break;
          }
        }
      }
      bbcode_invalid("bbcode: unbalanced tags: open [%s], close [/%s]\n", prev.name, name);            
      if (!has_open_tag) {
        bbcode_tag_push( bb, &prev ); // restore the tags and ignore this close tag
        break;
      }
      else {
        // continue until we hit our open tag
      }
    }
  }
  if (pprev != NULL) { memset(pprev,0,sizeof(*pprev)); }
  return false;
}

//-------------------------------------------------------------
// Update attributes
//-------------------------------------------------------------

static const char* attr_update_bool( const char* fname, signed int* field, const char* value ) {
  if (value == NULL || value[0] == 0 || strcmp(value,"on") || strcmp(value,"true") || strcmp(value,"1")) {
    *field = IC_ON;
  }
  else if (strcmp(value,"off") || strcmp(value,"false") || strcmp(value,"0")) {
    *field = IC_OFF;
  }
  else {
    bbcode_invalid("bbcode: invalid %s value: %s\n", fname, value );
  }
  return fname;
}

static const char* attr_update_color( const char* fname, ic_color_t* field, const char* value ) {
  if (value == NULL || value[0] == 0 || strcmp(value,"none") == 0) {
    *field = IC_COLOR_NONE;
    return fname;
  }
  
  // hex value
  if (value[0] == '#') {
    uint32_t rgb = 0;
    if (sscanf(value,"#%x",&rgb) == 1) {
      *field = ic_rgb(rgb);
    } 
    else {
      bbcode_invalid("bbcode: invalid color code: %s\n", value);
    }
    return fname;
  }

  // search color names
  ssize_t lo = 0;
  ssize_t hi = IC_HTML_COLOR_COUNT-1;
  while( lo <= hi ) {
    ssize_t mid = (lo + hi) / 2;
    style_color_t* info = &html_colors[mid];
    int cmp = strcmp(info->name,value);
    if (cmp < 0) {
      lo = mid+1;
    }
    else if (cmp > 0) {
      hi = mid-1;
    }
    else { 
      *field = info->color;
      return fname;
    }    
  }
  bbcode_invalid("bbcode: unknown %s: %s\n", fname, value);
  *field = IC_COLOR_NONE;
  return fname;
}

static const char* attr_update_sgr( const char* fname, attr_t* attr, const char* value ) {
  *attr = attr_update_with(*attr, attr_from_sgr(value, ic_strlen(value)));
  return fname;
}

static void attr_update_width( width_t* pwidth, char default_fill, const char* value ) {
  // parse width value: <width>;<left|center|right>;<fill>;<cut>
  width_t width;
  memset(&width, 0, sizeof(width));
  width.fill = default_fill;   // use 0 for no-fill (as for max-width)
  if (ic_atoz(value, &width.w)) {     
    ssize_t i = 0;
    while( value[i] != ';' && value[i] != 0 ) { i++; }
    if (value[i] == ';') {
      i++;
      ssize_t len = 0;    
      while( value[i+len] != ';' && value[i+len] != 0 ) { len++; }
      if (len == 4 && ic_istarts_with(value+i,"left")) {
        width.align = IC_ALIGN_LEFT;
      }
      else if (len == 5 && ic_istarts_with(value+i,"right")) {
        width.align = IC_ALIGN_RIGHT;
      }
      else if (len == 6 && ic_istarts_with(value+i,"center")) {
        width.align = IC_ALIGN_CENTER;
      }
      i += len;
      if (value[i] == ';') {
        i++; len = 0;
        while( value[i+len] != ';' && value[i+len] != 0 ) { len++; }
        if (len == 1) { width.fill = value[i]; }
        i+= len;
        if (value[i] == ';') {
          i++; len = 0;
          while( value[i+len] != ';' && value[i+len] != 0 ) { len++; }
          if ((len == 2 && ic_istarts_with(value+i,"on")) || (len == 1 && value[i] == '1')) { width.dots = true; }
          i += len;
        }
      }
    }
  }
  else {
    bbcode_invalid("bbcode: illegal width: %s\n", value);
  }
  *pwidth = width;
}

static const char* attr_update_ansi_color( const char* fname, ic_color_t* color, const char* value ) {
  ssize_t num = 0;
  if (ic_atoz(value, &num) && num >= 0 && num <= 256) {
    *color = color_from_ansi256(num);
  }
  return fname;
}


static const char* attr_update_property( tag_t* tag, const char* attr_name, const char* value ) {
  const char* fname = NULL;
  fname = "bold";
  if (strcmp(attr_name,fname) == 0) {    
    signed int b = IC_NONE;    
    attr_update_bool(fname,&b, value); 
    if (b != IC_NONE) { tag->attr.x.bold = b; }
    return fname;
  }
  fname = "italic";
  if (strcmp(attr_name,fname) == 0) {    
    signed int b = IC_NONE;      
    attr_update_bool(fname,&b, value); 
    if (b != IC_NONE) { tag->attr.x.italic = b; }
    return fname;
  }
  fname = "underline";
  if (strcmp(attr_name,fname) == 0) {  
    signed int b = IC_NONE;        
    attr_update_bool(fname,&b, value); 
    if (b != IC_NONE) { tag->attr.x.underline = b; }
    return fname;
  }
  fname = "reverse";
  if (strcmp(attr_name,fname) == 0) {
    signed int b = IC_NONE;          
    attr_update_bool(fname,&b, value); 
    if (b != IC_NONE) { tag->attr.x.reverse = b; }
    return fname;
  }
  fname = "color";
  if (strcmp(attr_name,fname) == 0) {
    unsigned int color = IC_COLOR_NONE;
    attr_update_color(fname, &color, value);
    if (color != IC_COLOR_NONE) { tag->attr.x.color = color; }
    return fname;
  }
  fname = "bgcolor";
  if (strcmp(attr_name,fname) == 0) {
    unsigned int color = IC_COLOR_NONE;
    attr_update_color(fname, &color, value);
    if (color != IC_COLOR_NONE) { tag->attr.x.bgcolor = color; }
    return fname;
  }
  fname = "ansi-sgr";
  if (strcmp(attr_name,fname) == 0) {
    attr_update_sgr(fname, &tag->attr, value);
    return fname;
  }
  fname = "ansi-color";
  if (strcmp(attr_name,fname) == 0) {
    ic_color_t color = IC_COLOR_NONE;;
    attr_update_ansi_color(fname, &color, value);
    if (color != IC_COLOR_NONE) { tag->attr.x.color = color; }
    return fname;
  }
  fname = "ansi-bgcolor";
  if (strcmp(attr_name,fname) == 0) {
    ic_color_t color = IC_COLOR_NONE;;
    attr_update_ansi_color(fname, &color, value);
    if (color != IC_COLOR_NONE) { tag->attr.x.bgcolor = color; }
    return fname;
  }  
  fname = "width";
  if (strcmp(attr_name,fname) == 0) {
    attr_update_width(&tag->width, ' ', value);
    return fname;
  }
  fname = "max-width";
  if (strcmp(attr_name,fname) == 0) {
    attr_update_width(&tag->width, 0, value);
    return "width";
  }    
  else {
    return NULL;
  }
}

static const style_t builtin_styles[] = {
  { "b",  { { IC_COLOR_NONE, IC_ON  , IC_NONE, IC_COLOR_NONE, IC_NONE, IC_NONE } } },
  { "r",  { { IC_COLOR_NONE, IC_NONE, IC_ON  , IC_COLOR_NONE, IC_NONE, IC_NONE } } },
  { "u",  { { IC_COLOR_NONE, IC_NONE, IC_NONE, IC_COLOR_NONE, IC_ON  , IC_NONE } } },
  { "i",  { { IC_COLOR_NONE, IC_NONE, IC_NONE, IC_COLOR_NONE, IC_NONE, IC_ON   } } },
  { "em", { { IC_COLOR_NONE, IC_ON  , IC_NONE, IC_COLOR_NONE, IC_NONE, IC_NONE } } }, // bold
  { "url",{ { IC_COLOR_NONE, IC_NONE, IC_NONE, IC_COLOR_NONE, IC_ON,   IC_NONE } } }, // underline
  { NULL, { { IC_COLOR_NONE, IC_NONE, IC_NONE, IC_COLOR_NONE, IC_NONE, IC_NONE } } }
};

static void attr_update_with_styles( tag_t* tag, const char* attr_name, const char* value, 
                                             bool usebgcolor, const style_t* styles, ssize_t count ) 
{
  // direct hex color?
  if (attr_name[0] == '#' && (value == NULL || value[0]==0)) {
    value = attr_name;
    attr_name = (usebgcolor ? "bgcolor" : "color");
  }
  // first try if it is a builtin property
  const char* name;
  if ((name = attr_update_property(tag,attr_name,value)) != NULL) {
    if (tag->name != NULL) tag->name = name;
    return;
  }
  // then check all styles
  while( count-- > 0 ) {
    const style_t* style = styles + count;
    if (strcmp(style->name,attr_name) == 0) {
      tag->attr = attr_update_with(tag->attr,style->attr);
      if (tag->name != NULL) tag->name = style->name;
      return;
    }    
  }
  // check builtin styles; todo: binary search?
  for( const style_t* style = builtin_styles; style->name != NULL; style++) {
    if (strcmp(style->name,attr_name) == 0) {
      tag->attr = attr_update_with(tag->attr,style->attr);
      if (tag->name != NULL) tag->name = style->name;
      return;
    }
  }
  // check colors as a style
  ssize_t lo = 0;
  ssize_t hi = IC_HTML_COLOR_COUNT-1;
  while( lo <= hi ) {
    ssize_t mid = (lo + hi) / 2;
    style_color_t* info = &html_colors[mid];
    int cmp = strcmp(info->name,attr_name);    
    if (cmp < 0) {
      lo = mid+1;
    }
    else if (cmp > 0) {
      hi = mid-1;
    }
    else {
      attr_t cattr = attr_none();
      if (usebgcolor) { cattr.x.bgcolor = info->color; }
                else  { cattr.x.color = info->color; }
      tag->attr = attr_update_with(tag->attr,cattr);
      if (tag->name != NULL) tag->name = info->name;
      return;
    }
  }
  // not found
  bbcode_invalid("bbcode: unknown style: %s\n", attr_name);  
}


ic_private attr_t bbcode_style( bbcode_t* bb, const char* style_name ) {
  tag_t tag;
  tag_init(&tag);
  attr_update_with_styles( &tag, style_name, NULL, false, bb->styles, bb->styles_count );
  return tag.attr;
}

//-------------------------------------------------------------
// Parse tags
//-------------------------------------------------------------

ic_private const char* parse_skip_white(const char* s) {
  while( *s != 0 && *s != ']') {
    if (!(*s == ' ' || *s == '\t' || *s == '\n' || *s == '\r')) break;
    s++;
  }
  return s;
}

ic_private const char* parse_skip_to_white(const char* s) {
  while( *s != 0 && *s != ']') {  
    if (*s == ' ' || *s == '\t' || *s == '\n' || *s == '\r') break;
    s++;
  }
  return parse_skip_white(s);
}

ic_private const char* parse_skip_to_end(const char* s) {
  while( *s != 0 && *s != ']' ) { s++; }    
  return s;
}

ic_private const char* parse_attr_name(const char* s) {
  if (*s == '#') {
    s++; // hex rgb color as id
    while( *s != 0 && *s != ']') {
      if (!((*s >= 'a' && *s <= 'f') || (*s >= 'A' && *s <= 'Z') || (*s >= '0' && *s <= '9'))) break;
      s++;
    }
  }
  else {
    while( *s != 0 && *s != ']') {
      if (!((*s >= 'a' && *s <= 'z') || (*s >= 'A' && *s <= 'Z') || 
            (*s >= '0' && *s <= '9') || *s == '_' || *s == '-')) break;
      s++;
    }
  }    
  return s;
}

ic_private const char* parse_value(const char* s, const char** start, const char** end) {
  if (*s == '"') {
    s++;
    *start = s;
    while( *s != 0 ) {
      if (*s == '"') break;
      s++;
    }
    *end = s;      
    if (*s == '"') { s++; }
  }
  else if (*s == '#') {
    *start = s;
    s++;
    while( *s != 0 ) {
      if (!((*s >= 'a' && *s <= 'f') || (*s >= 'A' && *s <= 'Z') || (*s >= '0' && *s <= '9'))) break;
      s++;
    }
    *end = s;
  }
  else {
    *start = s;
    while( *s != 0 ) {
      if (!((*s >= 'a' && *s <= 'z') || (*s >= 'A' && *s <= 'F') || (*s >= '0' && *s <= '9') || *s == '-' || *s == '_')) break;
      s++;
    }
    *end = s;
  }  
  return s;  
}

ic_private const char* parse_tag_value( tag_t* tag, char* idbuf, const char* s, const style_t* styles, ssize_t scount ) {
  // parse: \s*[\w-]+\s*(=\s*<value>)
  bool usebgcolor = false;
  const char* id = s;
  const char* idend = parse_attr_name(id);
  const char* val = NULL;
  const char* valend = NULL;  
  if (id == idend) {
    bbcode_invalid("bbcode: empty identifier? %.10s...\n", id );
    return parse_skip_to_white(id);
  }
  // "on" bgcolor?
  s = parse_skip_white(idend);
  if (idend - id == 2 && ic_strnicmp(id,"on",2) == 0 && *s != '=') {
    usebgcolor = true;
    id = s;
    idend = parse_attr_name(id);
    if (id == idend) {
      bbcode_invalid("bbcode: empty identifier follows 'on'? %.10s...\n", id );
      return parse_skip_to_white(id);
    }
    s = parse_skip_white(idend);      
  }
  // value
  if (*s == '=') {
    s++;
    s = parse_skip_white(s);
    s = parse_value(s, &val, &valend);
    s = parse_skip_white(s);
  }  
  // limit name and attr to 128 bytes
  char valbuf[128];
  ic_strncpy( idbuf, 128, id, idend - id);
  ic_strncpy( valbuf, 128, val, valend - val);
  ic_str_tolower(idbuf);
  ic_str_tolower(valbuf);
  attr_update_with_styles( tag, idbuf, valbuf, usebgcolor, styles, scount );  
  return s;
}

static const char* parse_tag_values( tag_t* tag, char* idbuf, const char* s, const style_t* styles, ssize_t scount ) {
  s = parse_skip_white(s);  
  idbuf[0] = 0;
  ssize_t count = 0;
  while( *s != 0 && *s != ']') {
    char idbuf_next[128];
    s = parse_tag_value(tag, (count==0 ? idbuf : idbuf_next), s, styles, scount);
    count++;
  }
  if (*s == ']') { s++; }
  return s;
}

static const char* parse_tag( tag_t* tag, char* idbuf, bool* open, bool* pre, const char* s, const style_t* styles, ssize_t scount ) {
  *open = true;
  *pre = false;
  if (*s != '[') return s;
  s = parse_skip_white(s+1);
  if (*s == '!') { // pre
    *pre = true;
    s = parse_skip_white(s+1);  
  }  
  else if (*s == '/') { 
    *open = false; 
    s = parse_skip_white(s+1); 
  };
  s = parse_tag_values( tag, idbuf, s, styles, scount);
  return s;
}


//---------------------------------------------------------
// Styles
//---------------------------------------------------------

static void bbcode_parse_tag_content( bbcode_t* bb, const char* s, tag_t* tag ) {
  tag_init(tag);
  if (s != NULL) { 
    char idbuf[128];
    parse_tag_values(tag, idbuf, s, bb->styles, bb->styles_count);
  }
}

ic_private void bbcode_style_def( bbcode_t* bb, const char* style_name, const char* s ) {
  tag_t tag;
  bbcode_parse_tag_content( bb, s, &tag);
  bbcode_style_add(bb, style_name, tag.attr);
}

ic_private void bbcode_style_open( bbcode_t* bb, const char* fmt ) {
  tag_t tag;
  bbcode_parse_tag_content(bb, fmt, &tag);
  term_set_attr( bb->term, bbcode_open(bb, 0, &tag, term_get_attr(bb->term)) );
}

ic_private void bbcode_style_close( bbcode_t* bb, const char* fmt ) {
  const ssize_t base = bb->tags_nesting - 1; // as we end a style
  tag_t tag;
  bbcode_parse_tag_content(bb, fmt, &tag);  
  tag_t prev;
  if (bbcode_close(bb, base, tag.name, &prev)) {
    term_set_attr( bb->term, prev.attr );
  }
}

//---------------------------------------------------------
// Restrict to width
//---------------------------------------------------------

static void bbcode_restrict_width( ssize_t start, width_t width, stringbuf_t* out, attrbuf_t* attr_out ) {
  if (width.w <= 0) return;
  assert(start <= sbuf_len(out));
  assert(attr_out == NULL || sbuf_len(out) == attrbuf_len(attr_out));
  const char*   s   = sbuf_string(out) + start;
  const ssize_t len = sbuf_len(out) - start;
  const ssize_t w   = str_column_width(s);
  if (w == width.w) return; // fits exactly
  if (w > width.w) {
    // too large
    ssize_t innerw = (width.dots && width.w > 3 ? width.w-3 : width.w);
    if (width.align == IC_ALIGN_RIGHT) {
      // right align
      const ssize_t ndel = str_skip_until_fit( s, innerw );
      sbuf_delete_at( out, start, ndel );
      attrbuf_delete_at( attr_out, start, ndel );
      if (innerw < width.w) {
        // add dots
        sbuf_insert_at( out, "...", start );
        attr_t attr = attrbuf_attr_at(attr_out, start);
        attrbuf_insert_at( attr_out, start, 3, attr);
      }
    }
    else {
      // left or center align
      ssize_t count = str_take_while_fit( s, innerw );
      sbuf_delete_at( out, start + count, len - count );
      attrbuf_delete_at( attr_out, start + count, len - count );
      if (innerw < width.w) {
        // add dots
        attr_t attr = attrbuf_attr_at(attr_out,start);
        attrbuf_append_n( out, attr_out, "...", 3, attr );
      }
    }
  }
  else {
    // too short, pad to width
    const ssize_t diff = (width.w - w);
    const ssize_t pad_left  = (width.align == IC_ALIGN_RIGHT ? diff : (width.align == IC_ALIGN_LEFT  ? 0 : diff / 2));
    const ssize_t pad_right = (width.align == IC_ALIGN_LEFT  ? diff : (width.align == IC_ALIGN_RIGHT ? 0 : diff - pad_left));
    if (width.fill != 0 && pad_left > 0) {
      const attr_t attr = attrbuf_attr_at(attr_out,start);
      for( ssize_t i = 0; i < pad_left; i++) {  // todo: optimize
        sbuf_insert_char_at(out, width.fill, start);
      }
      attrbuf_insert_at( attr_out, start, pad_left, attr );
    }
    if (width.fill != 0 && pad_right > 0) {
      const attr_t attr = attrbuf_attr_at(attr_out,sbuf_len(out) - 1);
      char buf[2];
      buf[0] = width.fill;
      buf[1] = 0;        
      for( ssize_t i = 0; i < pad_right; i++) {  // todo: optimize
        attrbuf_append_n( out, attr_out, buf, 1, attr );
      }      
    }
  }
}

//---------------------------------------------------------
// Print
//---------------------------------------------------------

ic_private ssize_t bbcode_process_tag( bbcode_t* bb, const char* s, const ssize_t nesting_base, 
                                        stringbuf_t* out, attrbuf_t* attr_out, attr_t* cur_attr ) {
  assert(*s == '[');
  tag_t tag;
  tag_init(&tag);  
  bool open = true;
  bool ispre = false;
  char idbuf[128];
  const char* end = parse_tag( &tag, idbuf, &open, &ispre, s, bb->styles, bb->styles_count ); // todo: styles
  assert(end > s);
  if (open) {
    if (!ispre) {
      // open tag
      *cur_attr = bbcode_open( bb, sbuf_len(out), &tag, *cur_attr );
    }
    else {
      // scan pre to end tag
      attr_t attr = attr_update_with(*cur_attr, tag.attr);
      char pre[132];
      if (snprintf(pre, 132, "[/%s]", idbuf) < ssizeof(pre)) {
        const char* etag = strstr(end,pre);
        if (etag == NULL) {
          const ssize_t len = ic_strlen(end);
          attrbuf_append_n(out, attr_out, end, len, attr);
          end += len;
        }
        else {
          attrbuf_append_n(out, attr_out, end, (etag - end), attr);
          end = etag + ic_strlen(pre);
        }
      }
    }
  }
  else {
    // pop the tag
    tag_t prev;
    if (bbcode_close( bb, nesting_base, tag.name, &prev)) {
      *cur_attr = prev.attr;
      if (prev.width.w > 0) {
        // closed a width tag; restrict the output to width
        bbcode_restrict_width( prev.pos, prev.width, out, attr_out);
      }
    }
  }  
  return (end - s);
}

ic_private void bbcode_append( bbcode_t* bb, const char* s, stringbuf_t* out, attrbuf_t* attr_out ) {
  if (bb == NULL || s == NULL) return;
  attr_t attr = attr_none();
  const ssize_t base = bb->tags_nesting; // base; will not be popped
  ssize_t i = 0;
  while( s[i] != 0 ) {
    // handle no tags in bulk
    ssize_t nobb = 0;
    char c;
    while( (c = s[i+nobb]) != 0) {
      if (c == '[' || c == '\\') { break; }
      if (c == '\x1B' && s[i+nobb+1] == '[') {
        nobb++; // don't count 'ESC[' as a tag opener
      }
      nobb++;
    }
    if (nobb > 0) { attrbuf_append_n(out, attr_out, s+i, nobb, attr); }
    i += nobb;
    // tag
    if (s[i] == '[') {
      i += bbcode_process_tag(bb, s+i, base, out, attr_out, &attr);
    }
    else if (s[i] == '\\') {
      if (s[i+1] == '\\' || s[i+1] == '[') {
        attrbuf_append_n(out, attr_out, s+i+1, 1, attr); // escape '\[' and '\\' 
        i += 2;
      }
      else {
        attrbuf_append_n(out, attr_out, s+i, 1, attr);  // pass '\\' as is
        i++;
      }
    }
  }
  // pop unclosed openings
  assert(bb->tags_nesting >= base);
  while( bb->tags_nesting > base ) {
    bbcode_tag_pop(bb,NULL);
  };
}

ic_private void bbcode_print( bbcode_t* bb, const char* s ) {
  if (bb->out == NULL || bb->out_attrs == NULL || s == NULL) return;
  assert(sbuf_len(bb->out) == 0 && attrbuf_len(bb->out_attrs) == 0);
  bbcode_append( bb, s, bb->out, bb->out_attrs );
  term_write_formatted( bb->term, sbuf_string(bb->out), attrbuf_attrs(bb->out_attrs,sbuf_len(bb->out)) );
  attrbuf_clear(bb->out_attrs);
  sbuf_clear(bb->out);
}

ic_private void bbcode_println( bbcode_t* bb, const char* s ) {
  bbcode_print(bb,s);
  term_writeln(bb->term, "");
}

ic_private void bbcode_vprintf( bbcode_t* bb, const char* fmt, va_list args  ) {
  if (bb->vout == NULL || fmt == NULL) return;
  assert(sbuf_len(bb->vout) == 0);
  sbuf_append_vprintf(bb->vout,fmt,args);
  bbcode_print(bb, sbuf_string(bb->vout));
  sbuf_clear(bb->vout);
}

ic_private void bbcode_printf( bbcode_t* bb, const char* fmt, ... ) {
  va_list args;
  va_start(args,fmt);
  bbcode_vprintf(bb,fmt,args);
  va_end(args);
}

ic_private ssize_t bbcode_column_width( bbcode_t* bb, const char* s ) {
  if (s==NULL || s[0] == 0) return 0;
  if (bb->vout == NULL) { return str_column_width(s); }
  assert(sbuf_len(bb->vout) == 0); 
  bbcode_append( bb, s, bb->vout, NULL);
  const ssize_t w = str_column_width(sbuf_string(bb->vout));
  sbuf_clear(bb->vout);
  return w;
}
