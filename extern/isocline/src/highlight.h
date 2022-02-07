/* ----------------------------------------------------------------------------
  Copyright (c) 2021, Daan Leijen
  This is free software; you can redistribute it and/or modify it
  under the terms of the MIT License. A copy of the license can be
  found in the "LICENSE" file at the root of this distribution.
-----------------------------------------------------------------------------*/
#pragma once
#ifndef IC_HIGHLIGHT_H
#define IC_HIGHLIGHT_H

#include "common.h"
#include "attr.h"
#include "term.h"
#include "bbcode.h"

//-------------------------------------------------------------
// Syntax highlighting
//-------------------------------------------------------------

ic_private void highlight( alloc_t* mem, bbcode_t* bb, const char* s, attrbuf_t* attrs, ic_highlight_fun_t* highlighter, void* arg );
ic_private void highlight_match_braces(const char* s, attrbuf_t* attrs, ssize_t cursor_pos, const char* braces, attr_t match_attr, attr_t error_attr);
ic_private ssize_t find_matching_brace(const char* s, ssize_t cursor_pos, const char* braces, bool* is_balanced);

#endif // IC_HIGHLIGHT_H
