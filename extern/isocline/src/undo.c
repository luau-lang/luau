/* ----------------------------------------------------------------------------
  Copyright (c) 2021, Daan Leijen
  This is free software; you can redistribute it and/or modify it
  under the terms of the MIT License. A copy of the license can be
  found in the "LICENSE" file at the root of this distribution.
-----------------------------------------------------------------------------*/
#include <string.h>
#include <stdio.h>

#include "../include/isocline.h"
#include "common.h"
#include "env.h"
#include "stringbuf.h"
#include "completions.h"
#include "undo.h"



//-------------------------------------------------------------
// edit state
//-------------------------------------------------------------
struct editstate_s {
  struct editstate_s* next;
  const char* input;          // input
  ssize_t     pos;            // cursor position
};

ic_private void editstate_init( editstate_t** es ) {
  *es = NULL;
}

ic_private void editstate_done( alloc_t* mem, editstate_t** es ) {
  while (*es != NULL) {
    editstate_t* next = (*es)->next;
    mem_free(mem, (*es)->input);
    mem_free(mem, *es );
    *es = next;
  }
  *es = NULL;
}

ic_private void editstate_capture( alloc_t* mem, editstate_t** es, const char* input, ssize_t pos) {
  if (input==NULL) input = "";
  // alloc
  editstate_t* entry = mem_zalloc_tp(mem, editstate_t);
  if (entry == NULL) return;
  // initialize
  entry->input = mem_strdup( mem, input);
  entry->pos   = pos;
  if (entry->input == NULL) { mem_free(mem, entry); return; }
  // and push
  entry->next = *es;
  *es = entry;
}

// caller should free *input
ic_private bool editstate_restore( alloc_t* mem, editstate_t** es, const char** input, ssize_t* pos ) {
  if (*es == NULL) return false;
  // pop 
  editstate_t* entry = *es;
  *es = entry->next;
  *input = entry->input;
  *pos = entry->pos;
  mem_free(mem, entry);
  return true;
}

