/* ----------------------------------------------------------------------------
  Copyright (c) 2021, Daan Leijen
  This is free software; you can redistribute it and/or modify it
  under the terms of the MIT License. A copy of the license can be
  found in the "LICENSE" file at the root of this distribution.
-----------------------------------------------------------------------------*/

//-------------------------------------------------------------
// History search: this file is included in editline.c
//-------------------------------------------------------------

static void edit_history_at(ic_env_t* env, editor_t* eb, int ofs ) 
{
  if (eb->modified) { 
    history_update(env->history, sbuf_string(eb->input)); // update first entry if modified
    eb->history_idx = 0;          // and start again 
    eb->modified = false;    
  }
  const char* entry = history_get(env->history,eb->history_idx + ofs);
  // debug_msg( "edit: history: at: %d + %d, found: %s\n", eb->history_idx, ofs, entry);
  if (entry == NULL) {
    term_beep(env->term);
  }
  else {
    eb->history_idx += ofs;
    sbuf_replace(eb->input, entry);
    if (ofs > 0) {
      // at end of first line when scrolling up
      ssize_t end = sbuf_find_line_end(eb->input,0);
      eb->pos = (end < 0 ? 0 : end);
    }
    else {
      eb->pos = sbuf_len(eb->input);    // at end of last line when scrolling down
    }
    edit_refresh(env, eb);
  }
}

static void edit_history_prev(ic_env_t* env, editor_t* eb) {
  edit_history_at(env,eb, 1 );
}

static void edit_history_next(ic_env_t* env, editor_t* eb) {
  edit_history_at(env,eb, -1 );
}

typedef struct hsearch_s {
  struct hsearch_s* next;
  ssize_t hidx;
  ssize_t match_pos;
  ssize_t match_len;
  bool cinsert;
} hsearch_t;

static void hsearch_push( alloc_t* mem, hsearch_t** hs, ssize_t hidx, ssize_t mpos, ssize_t mlen, bool cinsert ) {
  hsearch_t* h = mem_zalloc_tp( mem, hsearch_t );
  if (h == NULL) return;
  h->hidx = hidx;
  h->match_pos = mpos;
  h->match_len = mlen;
  h->cinsert = cinsert;
  h->next = *hs;
  *hs = h;
}

static bool hsearch_pop( alloc_t* mem, hsearch_t** hs, ssize_t* hidx, ssize_t* match_pos, ssize_t* match_len, bool* cinsert ) {
  hsearch_t* h = *hs;
  if (h == NULL) return false;
  *hs = h->next;
  if (hidx != NULL)      *hidx = h->hidx;
  if (match_pos != NULL) *match_pos = h->match_pos;
  if (match_len != NULL) *match_len = h->match_len;
  if (cinsert != NULL)   *cinsert = h->cinsert;
  mem_free(mem, h);
  return true;
}

static void hsearch_done( alloc_t* mem, hsearch_t* hs ) {
  while (hs != NULL) {
    hsearch_t* next = hs->next;
    mem_free(mem, hs);
    hs = next;
  }
}

static void edit_history_search(ic_env_t* env, editor_t* eb, char* initial ) {
  if (history_count( env->history ) <= 0) {
    term_beep(env->term);
    return;
  }

  // update history
  if (eb->modified) { 
    history_update(env->history, sbuf_string(eb->input)); // update first entry if modified
    eb->history_idx = 0;               // and start again 
    eb->modified = false;
  }

  // set a search prompt and remember the previous state
  editor_undo_capture(eb);
  eb->disable_undo = true;
  bool old_hint = ic_enable_hint(false);  
  const char* prompt_text = eb->prompt_text;
  eb->prompt_text = "history search";
  
  // search state
  hsearch_t* hs = NULL;        // search undo 
  ssize_t hidx = 1;            // current history entry
  ssize_t match_pos = 0;       // current matched position
  ssize_t match_len = 0;       // length of the match
  const char* hentry = NULL;   // current history entry
  
  // Simulate per character searches for each letter in `initial` (so backspace works)
  if (initial != NULL) {
    const ssize_t initial_len = ic_strlen(initial);
    ssize_t ipos = 0;
    while( ipos < initial_len ) {
      ssize_t next = str_next_ofs( initial, initial_len, ipos, NULL );
      if (next < 0) break;
      hsearch_push( eb->mem, &hs, hidx, match_pos, match_len, true);
      char c = initial[ipos + next];  // terminate temporarily
      initial[ipos + next] = 0;
      if (history_search( env->history, hidx, initial, true, &hidx, &match_pos )) {
        match_len = ipos + next;
      }      
      else if (ipos + next >= initial_len) {
        term_beep(env->term);
      }
      initial[ipos + next] = c;       // restore
      ipos += next;
    }
    sbuf_replace( eb->input, initial);
    eb->pos = ipos;
  }
  else {
    sbuf_clear( eb->input );
    eb->pos = 0;
  }

  // Incremental search
again:
  hentry = history_get(env->history,hidx);
  if (hentry != NULL) {
    sbuf_appendf(eb->extra, "[ic-info]%zd. [/][ic-diminish][!pre]", hidx);
    sbuf_append_n( eb->extra, hentry, match_pos );      
    sbuf_append(eb->extra, "[/pre][u ic-emphasis][!pre]" ); 
    sbuf_append_n( eb->extra, hentry + match_pos, match_len );
    sbuf_append(eb->extra, "[/pre][/u][!pre]" ); 
    sbuf_append(eb->extra, hentry + match_pos + match_len );
    sbuf_append(eb->extra, "[/pre][/ic-diminish]");
    if (!env->no_help) {
      sbuf_append(eb->extra, "\n[ic-info](use tab for the next match)[/]");
    }
    sbuf_append(eb->extra, "\n" );
  }
  edit_refresh(env, eb);

  // Wait for input
  code_t c = (hentry == NULL ? KEY_ESC : tty_read(env->tty));
  if (tty_term_resize_event(env->tty)) {
    edit_resize(env, eb);
  }
  sbuf_clear(eb->extra);

  // Process commands
  if (c == KEY_ESC || c == KEY_BELL /* ^G */ || c == KEY_CTRL_C) {
    c = 0;  
    eb->disable_undo = false;
    editor_undo_restore(eb, false);
  } 
  else if (c == KEY_ENTER) {
    c = 0;
    editor_undo_forget(eb);
    sbuf_replace( eb->input, hentry );
    eb->pos = sbuf_len(eb->input);
    eb->modified = false;
    eb->history_idx = hidx;
  }  
  else if (c == KEY_BACKSP || c == KEY_CTRL_Z) {
    // undo last search action
    bool cinsert;
    if (hsearch_pop(env->mem,&hs, &hidx, &match_pos, &match_len, &cinsert)) {
      if (cinsert) edit_backspace(env,eb);
    }
    goto again;
  }
  else if (c == KEY_CTRL_R || c == KEY_TAB || c == KEY_UP) {    
    // search backward
    hsearch_push(env->mem, &hs, hidx, match_pos, match_len, false);
    if (!history_search( env->history, hidx+1, sbuf_string(eb->input), true, &hidx, &match_pos )) {
      hsearch_pop(env->mem,&hs,NULL,NULL,NULL,NULL);
      term_beep(env->term);
    };
    goto again;
  }  
  else if (c == KEY_CTRL_S || c == KEY_SHIFT_TAB || c == KEY_DOWN) {    
    // search forward
    hsearch_push(env->mem, &hs, hidx, match_pos, match_len, false);
    if (!history_search( env->history, hidx-1, sbuf_string(eb->input), false, &hidx, &match_pos )) {
      hsearch_pop(env->mem, &hs,NULL,NULL,NULL,NULL);
      term_beep(env->term);
    };
    goto again;
  }
  else if (c == KEY_F1) {
    edit_show_help(env, eb);
    goto again;
  }
  else {
    // insert character and search further backward
    char chr;
    unicode_t uchr;
    if (code_is_ascii_char(c,&chr)) {
      hsearch_push(env->mem, &hs, hidx, match_pos, match_len, true);
      edit_insert_char(env,eb,chr);      
    }
    else if (code_is_unicode(c,&uchr)) {
      hsearch_push(env->mem, &hs, hidx, match_pos, match_len, true);
      edit_insert_unicode(env,eb,uchr);
    }
    else {
      // ignore command
      term_beep(env->term);
      goto again;
    }
    // search for the new input
    if (history_search( env->history, hidx, sbuf_string(eb->input), true, &hidx, &match_pos )) {
      match_len = sbuf_len(eb->input);
    }
    else {
      term_beep(env->term);
    };
    goto again;
  }

  // done
  eb->disable_undo = false;
  hsearch_done(env->mem,hs);
  eb->prompt_text = prompt_text;
  ic_enable_hint(old_hint);
  edit_refresh(env,eb);
  if (c != 0) tty_code_pushback(env->tty, c);
}

// Start an incremental search with the current word 
static void edit_history_search_with_current_word(ic_env_t* env, editor_t* eb) {
  char* initial = NULL;
  ssize_t start = sbuf_find_word_start( eb->input, eb->pos );
  if (start >= 0) {
    const ssize_t next = sbuf_next(eb->input, start, NULL);
    if (!ic_char_is_idletter(sbuf_string(eb->input) + start, (long)(next - start))) { 
      start = next; 
    }
    if (start >= 0 && start < eb->pos) {
      initial = mem_strndup(eb->mem, sbuf_string(eb->input) + start, eb->pos - start);
    }
  }
  edit_history_search( env, eb, initial);
  mem_free(env->mem, initial);
}
