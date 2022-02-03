/* ----------------------------------------------------------------------------
  Copyright (c) 2021, Daan Leijen
  This is free software; you can redistribute it and/or modify it
  under the terms of the MIT License. A copy of the license can be
  found in the "LICENSE" file at the root of this distribution.
-----------------------------------------------------------------------------*/
#pragma once
#ifndef IC_STRINGBUF_H
#define IC_STRINGBUF_H

#include <stdarg.h>
#include "common.h"

//-------------------------------------------------------------
// string buffer
// in-place modified buffer with edit operations 
// that grows on demand.
//-------------------------------------------------------------

// abstract string buffer
struct stringbuf_s;
typedef struct stringbuf_s stringbuf_t;

ic_private stringbuf_t*  sbuf_new( alloc_t* mem );
ic_private void    sbuf_free( stringbuf_t* sbuf );
ic_private char*   sbuf_free_dup(stringbuf_t* sbuf);
ic_private ssize_t sbuf_len(const stringbuf_t* s);

ic_private const char* sbuf_string_at( stringbuf_t* sbuf, ssize_t pos );
ic_private const char* sbuf_string( stringbuf_t* sbuf );
ic_private char    sbuf_char_at(stringbuf_t* sbuf, ssize_t pos);
ic_private char*   sbuf_strdup_at( stringbuf_t* sbuf, ssize_t pos );
ic_private char*   sbuf_strdup( stringbuf_t* sbuf );
ic_private char*   sbuf_strdup_from_utf8(stringbuf_t* sbuf);  // decode to locale


ic_private ssize_t sbuf_appendf(stringbuf_t* sb, const char* fmt, ...);
ic_private ssize_t sbuf_append_vprintf(stringbuf_t* sb, const char* fmt, va_list args);

ic_private stringbuf_t* sbuf_split_at( stringbuf_t* sb, ssize_t pos );

// primitive edit operations (inserts return the new position)
ic_private void    sbuf_clear(stringbuf_t* sbuf);
ic_private void    sbuf_replace(stringbuf_t* sbuf, const char* s);
ic_private void    sbuf_delete_at(stringbuf_t* sbuf, ssize_t pos, ssize_t count);
ic_private void    sbuf_delete_from_to(stringbuf_t* sbuf, ssize_t pos, ssize_t end);
ic_private void    sbuf_delete_from(stringbuf_t* sbuf, ssize_t pos );
ic_private ssize_t sbuf_insert_at_n(stringbuf_t* sbuf, const char* s, ssize_t n, ssize_t pos );
ic_private ssize_t sbuf_insert_at(stringbuf_t* sbuf, const char* s, ssize_t pos );
ic_private ssize_t sbuf_insert_char_at(stringbuf_t* sbuf, char c, ssize_t pos );
ic_private ssize_t sbuf_insert_unicode_at(stringbuf_t* sbuf, unicode_t u, ssize_t pos);
ic_private ssize_t sbuf_append_n(stringbuf_t* sbuf, const char* s, ssize_t n);
ic_private ssize_t sbuf_append(stringbuf_t* sbuf, const char* s);
ic_private ssize_t sbuf_append_char(stringbuf_t* sbuf, char c);

// high level edit operations (return the new position)
ic_private ssize_t sbuf_next( stringbuf_t* sbuf, ssize_t pos, ssize_t* cwidth );
ic_private ssize_t sbuf_prev( stringbuf_t* sbuf, ssize_t pos, ssize_t* cwidth );
ic_private ssize_t sbuf_next_ofs(stringbuf_t* sbuf, ssize_t pos, ssize_t* cwidth);

ic_private ssize_t sbuf_delete_char_before( stringbuf_t* sbuf, ssize_t pos );
ic_private void    sbuf_delete_char_at( stringbuf_t* sbuf, ssize_t pos );
ic_private ssize_t sbuf_swap_char( stringbuf_t* sbuf, ssize_t pos );

ic_private ssize_t sbuf_find_line_start( stringbuf_t* sbuf, ssize_t pos );
ic_private ssize_t sbuf_find_line_end( stringbuf_t* sbuf, ssize_t pos );
ic_private ssize_t sbuf_find_word_start( stringbuf_t* sbuf, ssize_t pos );
ic_private ssize_t sbuf_find_word_end( stringbuf_t* sbuf, ssize_t pos );
ic_private ssize_t sbuf_find_ws_word_start( stringbuf_t* sbuf, ssize_t pos );
ic_private ssize_t sbuf_find_ws_word_end( stringbuf_t* sbuf, ssize_t pos );

// parse a decimal 
ic_private bool ic_atoz(const char* s, ssize_t* i);
// parse two decimals separated by a semicolon
ic_private bool ic_atoz2(const char* s, ssize_t* i, ssize_t* j);
ic_private bool ic_atou32(const char* s, uint32_t* pu);

// row/column info
typedef struct rowcol_s {
  ssize_t row;
  ssize_t col;
  ssize_t row_start;
  ssize_t row_len;
  bool    first_on_row;
  bool    last_on_row;
} rowcol_t;

// find row/col position
ic_private ssize_t sbuf_get_pos_at_rc( stringbuf_t* sbuf, ssize_t termw, ssize_t promptw, ssize_t cpromptw, 
                                       ssize_t row, ssize_t col );
// get row/col for a given position
ic_private ssize_t sbuf_get_rc_at_pos( stringbuf_t* sbuf, ssize_t termw, ssize_t promptw, ssize_t cpromptw, 
                                       ssize_t pos, rowcol_t* rc );

ic_private ssize_t sbuf_get_wrapped_rc_at_pos( stringbuf_t* sbuf, ssize_t termw, ssize_t newtermw, ssize_t promptw, ssize_t cpromptw, 
                                       ssize_t pos, rowcol_t* rc );
                                       
// row iteration
typedef bool (row_fun_t)(const char* s,
                          ssize_t row, ssize_t row_start, ssize_t row_len, 
                          ssize_t startw, // prompt width
                          bool is_wrap, const void* arg, void* res);

ic_private ssize_t sbuf_for_each_row( stringbuf_t* sbuf, ssize_t termw, ssize_t promptw, ssize_t cpromptw, 
                                      row_fun_t* fun, void* arg, void* res );


//-------------------------------------------------------------
// Strings
//-------------------------------------------------------------

// skip a single CSI sequence (ESC [ ...)
ic_private bool    skip_csi_esc( const char* s, ssize_t len, ssize_t* esclen ); // used in term.c

ic_private ssize_t str_column_width( const char* s );
ic_private ssize_t str_prev_ofs( const char* s, ssize_t pos, ssize_t* cwidth );
ic_private ssize_t str_next_ofs( const char* s, ssize_t len, ssize_t pos, ssize_t* cwidth );
ic_private ssize_t str_skip_until_fit( const char* s, ssize_t max_width);  // tail that fits
ic_private ssize_t str_take_while_fit( const char* s, ssize_t max_width);  // prefix that fits

#endif // IC_STRINGBUF_H
