/* ----------------------------------------------------------------------------
  Copyright (c) 2021, Daan Leijen
  This is free software; you can redistribute it and/or modify it
  under the terms of the MIT License. A copy of the license can be
  found in the "LICENSE" file at the root of this distribution.
-----------------------------------------------------------------------------*/
#pragma once
#ifndef IC_COMPLETIONS_H
#define IC_COMPLETIONS_H

#include "common.h"
#include "stringbuf.h"


//-------------------------------------------------------------
// Completions
//-------------------------------------------------------------
#define IC_MAX_COMPLETIONS_TO_SHOW  (1000)
#define IC_MAX_COMPLETIONS_TO_TRY   (IC_MAX_COMPLETIONS_TO_SHOW/4)

typedef struct completions_s completions_t;

ic_private completions_t* completions_new(alloc_t* mem);
ic_private void        completions_free(completions_t* cms);
ic_private void        completions_clear(completions_t* cms);
ic_private bool        completions_add(completions_t* cms , const char* replacement, const char* display, const char* help, ssize_t delete_before, ssize_t delete_after);
ic_private ssize_t     completions_count(completions_t* cms);
ic_private ssize_t     completions_generate(struct ic_env_s* env, completions_t* cms , const char* input, ssize_t pos, ssize_t max);
ic_private void        completions_sort(completions_t* cms);
ic_private void        completions_set_completer(completions_t* cms, ic_completer_fun_t* completer, void* arg);
ic_private const char* completions_get_display(completions_t* cms , ssize_t index, const char** help);
ic_private const char* completions_get_hint(completions_t* cms, ssize_t index, const char** help);
ic_private void        completions_get_completer(completions_t* cms, ic_completer_fun_t** completer, void** arg);

ic_private ssize_t     completions_apply(completions_t* cms, ssize_t index, stringbuf_t* sbuf, ssize_t pos);
ic_private ssize_t     completions_apply_longest_prefix(completions_t* cms, stringbuf_t* sbuf, ssize_t pos);

//-------------------------------------------------------------
// Completion environment
//-------------------------------------------------------------
typedef bool (ic_completion_fun_t)( ic_env_t* env, void* funenv, const char* replacement, const char* display, const char* help, long delete_before, long delete_after );

struct ic_completion_env_s {
  ic_env_t*   env;       // the isocline environment
  const char* input;     // current full input
  long        cursor;    // current cursor position
  void*       arg;       // argument given to `ic_set_completer`
  void*       closure;   // free variables for function composition
  ic_completion_fun_t* complete;  // function that adds a completion
};

#endif // IC_COMPLETIONS_H
