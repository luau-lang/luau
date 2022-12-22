/* ----------------------------------------------------------------------------
  Copyright (c) 2021, Daan Leijen
  This is free software; you can redistribute it and/or modify it
  under the terms of the MIT License. A copy of the license can be
  found in the "LICENSE" file at the root of this distribution.
-----------------------------------------------------------------------------*/
#pragma once
#ifndef IC_BBCODE_H
#define IC_BBCODE_H

#include <stdarg.h>
#include "common.h"
#include "term.h"

struct bbcode_s;
typedef struct bbcode_s bbcode_t;

ic_private bbcode_t* bbcode_new( alloc_t* mem, term_t* term );
ic_private void bbcode_free( bbcode_t* bb );

ic_private void bbcode_style_add( bbcode_t* bb, const char* style_name, attr_t attr );
ic_private void bbcode_style_def( bbcode_t* bb, const char* style_name, const char* s );
ic_private void bbcode_style_open( bbcode_t* bb, const char* fmt );
ic_private void bbcode_style_close( bbcode_t* bb, const char* fmt );
ic_private attr_t bbcode_style( bbcode_t* bb, const char* style_name );

ic_private void bbcode_print( bbcode_t* bb, const char* s );
ic_private void bbcode_println( bbcode_t* bb, const char* s );
ic_private void bbcode_printf( bbcode_t* bb, const char* fmt, ... );
ic_private void bbcode_vprintf( bbcode_t* bb, const char* fmt, va_list args );

ic_private ssize_t bbcode_column_width( bbcode_t* bb, const char* s );

// allows `attr_out == NULL`.
ic_private void bbcode_append( bbcode_t* bb, const char* s, stringbuf_t* out, attrbuf_t* attr_out );

#endif // IC_BBCODE_H
