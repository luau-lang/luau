/* ----------------------------------------------------------------------------
  Copyright (c) 2021, Daan Leijen
  This is free software; you can redistribute it and/or modify it
  under the terms of the MIT License. A copy of the license can be
  found in the "LICENSE" file at the root of this distribution.
-----------------------------------------------------------------------------*/
#include <stdio.h>
#include <string.h>

#include "common.h"
#include "term.h"
#include "tty.h"
#include "env.h"
#include "stringbuf.h"
#include "history.h"
#include "completions.h"
#include "undo.h"
#include "highlight.h"

//-------------------------------------------------------------
// The editor state
//-------------------------------------------------------------



// editor state
typedef struct editor_s {
  stringbuf_t*  input;        // current user input
  stringbuf_t*  extra;        // extra displayed info (for completion menu etc)
  stringbuf_t*  hint;         // hint displayed as part of the input
  stringbuf_t*  hint_help;    // help for a hint.
  ssize_t       pos;          // current cursor position in the input
  ssize_t       cur_rows;     // current used rows to display our content (including extra content)
  ssize_t       cur_row;      // current row that has the cursor (0 based, relative to the prompt)
  ssize_t       termw;
  bool          modified;     // has a modification happened? (used for history navigation for example)  
  bool          disable_undo; // temporarily disable auto undo (for history search)
  ssize_t       history_idx;  // current index in the history 
  editstate_t*  undo;         // undo buffer  
  editstate_t*  redo;         // redo buffer
  const char*   prompt_text;  // text of the prompt before the prompt marker    
  alloc_t*      mem;          // allocator
  // caches
  attrbuf_t*    attrs;        // reuse attribute buffers 
  attrbuf_t*    attrs_extra; 
} editor_t;





//-------------------------------------------------------------
// Main edit line 
//-------------------------------------------------------------
static char* edit_line( ic_env_t* env, const char* prompt_text );  // defined at bottom
static void edit_refresh(ic_env_t* env, editor_t* eb);

ic_private char* ic_editline(ic_env_t* env, const char* prompt_text) {
  tty_start_raw(env->tty);
  term_start_raw(env->term);
  char* line = edit_line(env,prompt_text);
  term_end_raw(env->term,false);
  tty_end_raw(env->tty);
  term_writeln(env->term,"");
  term_flush(env->term);
  return line;
}


//-------------------------------------------------------------
// Undo/Redo
//-------------------------------------------------------------

// capture the current edit state
static void editor_capture(editor_t* eb, editstate_t** es ) {
  if (!eb->disable_undo) {
    editstate_capture( eb->mem, es, sbuf_string(eb->input), eb->pos );
  }
}

static void editor_undo_capture(editor_t* eb ) {
  editor_capture(eb, &eb->undo );
}

static void editor_undo_forget(editor_t* eb) {
  if (eb->disable_undo) return;
  const char* input = NULL;
  ssize_t pos = 0;
  editstate_restore(eb->mem, &eb->undo, &input, &pos);
  mem_free(eb->mem, input);
}

static void editor_restore(editor_t* eb, editstate_t** from, editstate_t** to ) {
  if (eb->disable_undo) return;
  if (*from == NULL) return;
  const char* input;
  if (to != NULL) { editor_capture( eb, to ); }
  if (!editstate_restore( eb->mem, from, &input, &eb->pos )) return;
  sbuf_replace( eb->input, input );
  mem_free(eb->mem, input);
  eb->modified = false;
}

static void editor_undo_restore(editor_t* eb, bool with_redo ) {
  editor_restore(eb, &eb->undo, (with_redo ? &eb->redo : NULL));
}

static void editor_redo_restore(editor_t* eb ) {
  editor_restore(eb, &eb->redo, &eb->undo);
  eb->modified = false;
}

static void editor_start_modify(editor_t* eb ) {
  editor_undo_capture(eb);
  editstate_done(eb->mem, &eb->redo);  // clear redo
  eb->modified = true;
}



static bool editor_pos_is_at_end(editor_t* eb ) {
  return (eb->pos == sbuf_len(eb->input));  
}

//-------------------------------------------------------------
// Row/Column width and positioning
//-------------------------------------------------------------


static void edit_get_prompt_width( ic_env_t* env, editor_t* eb, bool in_extra, ssize_t* promptw, ssize_t* cpromptw ) {
  if (in_extra) {
    *promptw = 0;
    *cpromptw = 0;
  }
  else {
    // todo: cache prompt widths
    ssize_t textw = bbcode_column_width(env->bbcode, eb->prompt_text);
    ssize_t markerw = bbcode_column_width(env->bbcode, env->prompt_marker);
    ssize_t cmarkerw = bbcode_column_width(env->bbcode, env->cprompt_marker);
    *promptw = markerw + textw;
    *cpromptw = (env->no_multiline_indent || *promptw < cmarkerw ? cmarkerw : *promptw);
  }
}

static ssize_t edit_get_rowcol( ic_env_t* env, editor_t* eb, rowcol_t* rc ) {
  ssize_t promptw, cpromptw;
  edit_get_prompt_width(env, eb, false, &promptw, &cpromptw);
  return sbuf_get_rc_at_pos( eb->input, eb->termw, promptw, cpromptw, eb->pos, rc );
}

static void edit_set_pos_at_rowcol( ic_env_t* env, editor_t* eb, ssize_t row, ssize_t col ) {
  ssize_t promptw, cpromptw;
  edit_get_prompt_width(env, eb, false, &promptw, &cpromptw);
  ssize_t pos = sbuf_get_pos_at_rc( eb->input, eb->termw, promptw, cpromptw, row, col );
  if (pos < 0) return;
  eb->pos = pos;
  edit_refresh(env, eb);
}

static bool edit_pos_is_at_row_end( ic_env_t* env, editor_t* eb ) {
  rowcol_t rc;
  edit_get_rowcol( env, eb, &rc );
  return rc.last_on_row;
}

static void edit_write_prompt( ic_env_t* env, editor_t* eb, ssize_t row, bool in_extra ) {
  if (in_extra) return;
  bbcode_style_open(env->bbcode, "ic-prompt");
  if (row==0) {
    // regular prompt text    
    bbcode_print( env->bbcode, eb->prompt_text );
  }
  else if (!env->no_multiline_indent) {
    // multiline continuation indentation
    // todo: cache prompt widths
    ssize_t textw = bbcode_column_width(env->bbcode, eb->prompt_text );
    ssize_t markerw = bbcode_column_width(env->bbcode, env->prompt_marker);
    ssize_t cmarkerw = bbcode_column_width(env->bbcode, env->cprompt_marker);      
    if (cmarkerw < markerw + textw) {
      term_write_repeat(env->term, " ", markerw + textw - cmarkerw );
    }
  }
  // the marker
  bbcode_print(env->bbcode, (row == 0 ? env->prompt_marker : env->cprompt_marker ));   
  bbcode_style_close(env->bbcode,NULL);    
}

//-------------------------------------------------------------
// Refresh
//-------------------------------------------------------------

typedef struct refresh_info_s {
  ic_env_t*   env;
  editor_t*   eb;
  attrbuf_t*  attrs;
  bool        in_extra;
  ssize_t     first_row;
  ssize_t     last_row;
} refresh_info_t;

static bool edit_refresh_rows_iter(
    const char* s,
    ssize_t row, ssize_t row_start, ssize_t row_len, 
    ssize_t startw, bool is_wrap, const void* arg, void* res)
{
  ic_unused(res); ic_unused(startw);
  const refresh_info_t* info = (const refresh_info_t*)(arg);
  term_t* term = info->env->term;

  // debug_msg("edit: line refresh: row %zd, len: %zd\n", row, row_len);
  if (row < info->first_row) return false;
  if (row > info->last_row)  return true; // should not occur
  
  // term_clear_line(term);
  edit_write_prompt(info->env, info->eb, row, info->in_extra);

  //' write output
  if (info->attrs == NULL || (info->env->no_highlight && info->env->no_bracematch)) {
    term_write_n( term, s + row_start, row_len );
  }
  else {
    term_write_formatted_n( term, s + row_start, attrbuf_attrs(info->attrs, row_start + row_len) + row_start, row_len );
  }

  // write line ending
  if (row < info->last_row) {
    if (is_wrap && tty_is_utf8(info->env->tty)) {       
      #ifndef __APPLE__
      bbcode_print( info->env->bbcode, "[ic-dim]\xE2\x86\x90");  // left arrow 
      #else
      bbcode_print( info->env->bbcode, "[ic-dim]\xE2\x86\xB5" ); // return symbol
      #endif
    }
    term_clear_to_end_of_line(term);
    term_writeln(term, "");
  }
  else {
    term_clear_to_end_of_line(term);
  }
  return (row >= info->last_row);  
}

static void edit_refresh_rows(ic_env_t* env, editor_t* eb, stringbuf_t* input, attrbuf_t* attrs,
                               ssize_t promptw, ssize_t cpromptw, bool in_extra, 
                                ssize_t first_row, ssize_t last_row) 
{
  if (input == NULL) return;
  refresh_info_t info;
  info.env        = env;
  info.eb         = eb;
  info.attrs      = attrs;
  info.in_extra   = in_extra;
  info.first_row  = first_row;
  info.last_row   = last_row;
  sbuf_for_each_row( input, eb->termw, promptw, cpromptw, &edit_refresh_rows_iter, &info, NULL);
}


static void edit_refresh(ic_env_t* env, editor_t* eb) 
{
  // calculate the new cursor row and total rows needed
  ssize_t promptw, cpromptw;
  edit_get_prompt_width( env, eb, false, &promptw, &cpromptw );
  
  if (eb->attrs != NULL) {
    highlight( env->mem, env->bbcode, sbuf_string(eb->input), eb->attrs, 
                 (env->no_highlight ? NULL : env->highlighter), env->highlighter_arg );
  }

  // highlight matching braces
  if (eb->attrs != NULL && !env->no_bracematch) {
    highlight_match_braces(sbuf_string(eb->input), eb->attrs, eb->pos, ic_env_get_match_braces(env),  
                              bbcode_style(env->bbcode,"ic-bracematch"), bbcode_style(env->bbcode,"ic-error"));
  }

  // insert hint  
  if (sbuf_len(eb->hint) > 0) {
    if (eb->attrs != NULL) {
      attrbuf_insert_at( eb->attrs, eb->pos, sbuf_len(eb->hint), bbcode_style(env->bbcode, "ic-hint") );
    }
    sbuf_insert_at(eb->input, sbuf_string(eb->hint), eb->pos );
  }

  // render extra (like a completion menu)
  stringbuf_t* extra = NULL;
  if (sbuf_len(eb->extra) > 0) {
    extra = sbuf_new(eb->mem);
    if (extra != NULL) {
      if (sbuf_len(eb->hint_help) > 0) {
        bbcode_append(env->bbcode, sbuf_string(eb->hint_help), extra, eb->attrs_extra);
      }
      bbcode_append(env->bbcode, sbuf_string(eb->extra), extra, eb->attrs_extra);
    }
  }

  // calculate rows and row/col position
  rowcol_t rc = { 0 };
  const ssize_t rows_input = sbuf_get_rc_at_pos( eb->input, eb->termw, promptw, cpromptw, eb->pos, &rc );
  rowcol_t rc_extra = { 0 };
  ssize_t rows_extra = 0;
  if (extra != NULL) { 
    rows_extra = sbuf_get_rc_at_pos( extra, eb->termw, 0, 0, 0 /*pos*/, &rc_extra ); 
  }
  const ssize_t rows = rows_input + rows_extra; 
  debug_msg("edit: refresh: rows %zd, cursor: %zd,%zd (previous rows %zd, cursor row %zd)\n", rows, rc.row, rc.col, eb->cur_rows, eb->cur_row);
  
  // only render at most terminal height rows
  const ssize_t termh = term_get_height(env->term);
  ssize_t first_row = 0;                 // first visible row 
  ssize_t last_row = rows - 1;           // last visible row
  if (rows > termh) {
    first_row = rc.row - termh + 1;      // ensure cursor is visible
    if (first_row < 0) first_row = 0;
    last_row = first_row + termh - 1;
  }
  assert(last_row - first_row < termh);
  
  // reduce flicker
  buffer_mode_t bmode = term_set_buffer_mode(env->term, BUFFERED);        

  // back up to the first line
  term_start_of_line(env->term);
  term_up(env->term, (eb->cur_row >= termh ? termh-1 : eb->cur_row) );
  // term_clear_lines_to_end(env->term);  // gives flicker in old Windows cmd prompt 

  // render rows
  edit_refresh_rows( env, eb, eb->input, eb->attrs, promptw, cpromptw, false, first_row, last_row );  
  if (rows_extra > 0) {
    assert(extra != NULL);
    const ssize_t first_rowx = (first_row > rows_input ? first_row - rows_input : 0);
    const ssize_t last_rowx = last_row - rows_input; assert(last_rowx >= 0);
    edit_refresh_rows(env, eb, extra, eb->attrs_extra, 0, 0, true, first_rowx, last_rowx);
  }
    
  // overwrite trailing rows we do not use anymore  
  ssize_t rrows = last_row - first_row + 1;  // rendered rows
  if (rrows < termh && rows < eb->cur_rows) {
    ssize_t clear = eb->cur_rows - rows;
    while (rrows < termh && clear > 0) {
      clear--;
      rrows++;
      term_writeln(env->term,"");
      term_clear_line(env->term);
    }
  }
  
  // move cursor back to edit position
  term_start_of_line(env->term);
  term_up(env->term, first_row + rrows - 1 - rc.row );
  term_right(env->term, rc.col + (rc.row == 0 ? promptw : cpromptw));

  // and refresh
  term_flush(env->term);

  // stop buffering
  term_set_buffer_mode(env->term, bmode);

  // restore input by removing the hint
  sbuf_delete_at(eb->input, eb->pos, sbuf_len(eb->hint));
  sbuf_delete_at(eb->extra, 0, sbuf_len(eb->hint_help));
  attrbuf_clear(eb->attrs);
  attrbuf_clear(eb->attrs_extra);
  sbuf_free(extra);

  // update previous
  eb->cur_rows = rows;
  eb->cur_row = rc.row;
}

// clear current output
static void edit_clear(ic_env_t* env, editor_t* eb ) {
  term_attr_reset(env->term);  
  term_up(env->term, eb->cur_row);
  
  // overwrite all rows
  for( ssize_t i = 0; i < eb->cur_rows; i++) {
    term_clear_line(env->term);
    term_writeln(env->term, "");    
  }
  
  // move cursor back 
  term_up(env->term, eb->cur_rows - eb->cur_row );  
}


// clear screen and refresh
static void edit_clear_screen(ic_env_t* env, editor_t* eb ) {
  ssize_t cur_rows = eb->cur_rows;
  eb->cur_rows = term_get_height(env->term) - 1;
  edit_clear(env,eb);
  eb->cur_rows = cur_rows;
  edit_refresh(env,eb);
}


// refresh after a terminal window resized (but before doing further edit operations!)
static bool edit_resize(ic_env_t* env, editor_t* eb ) {
  // update dimensions
  term_update_dim(env->term);
  ssize_t newtermw = term_get_width(env->term);
  if (eb->termw == newtermw) return false;
  
  // recalculate the row layout assuming the hardwrapping for the new terminal width
  ssize_t promptw, cpromptw;
  edit_get_prompt_width( env, eb, false, &promptw, &cpromptw );
  sbuf_insert_at(eb->input, sbuf_string(eb->hint), eb->pos); // insert used hint    
  
  // render extra (like a completion menu)
  stringbuf_t* extra = NULL;
  if (sbuf_len(eb->extra) > 0) {
    extra = sbuf_new(eb->mem);
    if (extra != NULL) {
      if (sbuf_len(eb->hint_help) > 0) {
        bbcode_append(env->bbcode, sbuf_string(eb->hint_help), extra, NULL);
      }
      bbcode_append(env->bbcode, sbuf_string(eb->extra), extra, NULL);
    }
  }
  rowcol_t rc = { 0 };
  const ssize_t rows_input = sbuf_get_wrapped_rc_at_pos( eb->input, eb->termw, newtermw, promptw, cpromptw, eb->pos, &rc );
  rowcol_t rc_extra = { 0 };
  ssize_t rows_extra = 0;
  if (extra != NULL) {
    rows_extra = sbuf_get_wrapped_rc_at_pos(extra, eb->termw, newtermw, 0, 0, 0 /*pos*/, &rc_extra);
  }  
  ssize_t rows = rows_input + rows_extra;
  debug_msg("edit: resize: new rows: %zd, cursor row: %zd (previous: rows: %zd, cursor row %zd)\n", rows, rc.row, eb->cur_rows, eb->cur_row);
  
  // update the newly calculated row and rows
  eb->cur_row = rc.row;
  if (rows > eb->cur_rows) {
    eb->cur_rows = rows;
  }
  eb->termw = newtermw;     
  edit_refresh(env,eb); 

  // remove hint again
  sbuf_delete_at(eb->input, eb->pos, sbuf_len(eb->hint));
  sbuf_free(extra);
  return true;
} 

static void editor_append_hint_help(editor_t* eb, const char* help) {
  sbuf_clear(eb->hint_help);
  if (help != NULL) {
    sbuf_replace(eb->hint_help, "[ic-info]");
    sbuf_append(eb->hint_help, help);
    sbuf_append(eb->hint_help, "[/ic-info]\n");
  }
}

// refresh with possible hint
static void edit_refresh_hint(ic_env_t* env, editor_t* eb) {
  if (env->no_hint || env->hint_delay > 0) {
    // refresh without hint first
    edit_refresh(env, eb);
    if (env->no_hint) return;
  }
    
  // and see if we can construct a hint (displayed after a delay)
  ssize_t count = completions_generate(env, env->completions, sbuf_string(eb->input), eb->pos, 2);
  if (count == 1) {
    const char* help = NULL;
    const char* hint = completions_get_hint(env->completions, 0, &help);
    if (hint != NULL) {
      sbuf_replace(eb->hint, hint); 
      editor_append_hint_help(eb, help);
      // do auto-tabbing?
      if (env->complete_autotab) {
        stringbuf_t* sb = sbuf_new(env->mem);  // temporary buffer for completion
        if (sb != NULL) { 
          sbuf_replace( sb, sbuf_string(eb->input) ); 
          ssize_t pos = eb->pos;
          const char* extra_hint = hint;
          do {
            ssize_t newpos = sbuf_insert_at( sb, extra_hint, pos );
            if (newpos <= pos) break;
            pos = newpos;
            count = completions_generate(env, env->completions, sbuf_string(sb), pos, 2);
            if (count == 1) {
              const char* extra_help = NULL;
              extra_hint = completions_get_hint(env->completions, 0, &extra_help);
              if (extra_hint != NULL) {
                editor_append_hint_help(eb, extra_help);
                sbuf_append(eb->hint, extra_hint);
              }
            }
          }
          while(count == 1);       
          sbuf_free(sb);
        }          
      }      
    }
  }

  if (env->hint_delay <= 0) {
    // refresh with hint directly
    edit_refresh(env, eb);
  }
}

//-------------------------------------------------------------
// Edit operations
//-------------------------------------------------------------

static void edit_history_prev(ic_env_t* env, editor_t* eb);
static void edit_history_next(ic_env_t* env, editor_t* eb);

static void edit_undo_restore(ic_env_t* env, editor_t* eb) {
  editor_undo_restore(eb, true);
  edit_refresh(env,eb);
}

static void edit_redo_restore(ic_env_t* env, editor_t* eb) {
  editor_redo_restore(eb);
  edit_refresh(env,eb);
}

static void edit_cursor_left(ic_env_t* env, editor_t* eb) {
  ssize_t cwidth = 1;
  ssize_t prev = sbuf_prev(eb->input,eb->pos,&cwidth);
  if (prev < 0) return;
  rowcol_t rc;
  edit_get_rowcol( env, eb, &rc);  
  eb->pos = prev;  
  edit_refresh(env,eb);  
}

static void edit_cursor_right(ic_env_t* env, editor_t* eb) {
  ssize_t cwidth = 1;
  ssize_t next = sbuf_next(eb->input,eb->pos,&cwidth);
  if (next < 0) return;
  rowcol_t rc;
  edit_get_rowcol( env, eb, &rc);  
  eb->pos = next;  
  edit_refresh(env,eb);
}

static void edit_cursor_line_end(ic_env_t* env, editor_t* eb) {
  ssize_t end = sbuf_find_line_end(eb->input,eb->pos);
  if (end < 0) return;  
  eb->pos = end; 
  edit_refresh(env,eb);
}

static void edit_cursor_line_start(ic_env_t* env, editor_t* eb) {
  ssize_t start = sbuf_find_line_start(eb->input,eb->pos);
  if (start < 0) return;
  eb->pos = start;
  edit_refresh(env,eb);
}

static void edit_cursor_next_word(ic_env_t* env, editor_t* eb) {
  ssize_t end = sbuf_find_word_end(eb->input,eb->pos);
  if (end < 0) return;
  eb->pos = end;
  edit_refresh(env,eb);
}

static void edit_cursor_prev_word(ic_env_t* env, editor_t* eb) {
  ssize_t start = sbuf_find_word_start(eb->input,eb->pos);
  if (start < 0) return;
  eb->pos = start;
  edit_refresh(env,eb);
}

static void edit_cursor_next_ws_word(ic_env_t* env, editor_t* eb) {
  ssize_t end = sbuf_find_ws_word_end(eb->input, eb->pos);
  if (end < 0) return;
  eb->pos = end;
  edit_refresh(env, eb);
}

static void edit_cursor_prev_ws_word(ic_env_t* env, editor_t* eb) {
  ssize_t start = sbuf_find_ws_word_start(eb->input, eb->pos);
  if (start < 0) return;
  eb->pos = start;
  edit_refresh(env, eb);
}

static void edit_cursor_to_start(ic_env_t* env, editor_t* eb) {
  eb->pos = 0; 
  edit_refresh(env,eb);
}

static void edit_cursor_to_end(ic_env_t* env, editor_t* eb) {
  eb->pos = sbuf_len(eb->input); 
  edit_refresh(env,eb);
}


static void edit_cursor_row_up(ic_env_t* env, editor_t* eb) {
  rowcol_t rc;
  edit_get_rowcol( env, eb, &rc);
  if (rc.row == 0) {
    edit_history_prev(env,eb);
  }
  else {
    edit_set_pos_at_rowcol( env, eb, rc.row - 1, rc.col );
  }
}

static void edit_cursor_row_down(ic_env_t* env, editor_t* eb) {
  rowcol_t rc;
  ssize_t rows = edit_get_rowcol( env, eb, &rc);
  if (rc.row + 1 >= rows) {
    edit_history_next(env,eb);
  }
  else {
    edit_set_pos_at_rowcol( env, eb, rc.row + 1, rc.col );
  }
}


static void edit_cursor_match_brace(ic_env_t* env, editor_t* eb) {
  ssize_t match = find_matching_brace( sbuf_string(eb->input), eb->pos, ic_env_get_match_braces(env), NULL );
  if (match < 0) return;
  eb->pos = match;
  edit_refresh(env,eb);
}

static void edit_backspace(ic_env_t* env, editor_t* eb) {
  if (eb->pos <= 0) return;
  editor_start_modify(eb);
  eb->pos = sbuf_delete_char_before(eb->input,eb->pos);
  edit_refresh(env,eb);
}

static void edit_delete_char(ic_env_t* env, editor_t* eb) {
  if (eb->pos >= sbuf_len(eb->input)) return;
  editor_start_modify(eb);
  sbuf_delete_char_at(eb->input,eb->pos);
  edit_refresh(env,eb);
}

static void edit_delete_all(ic_env_t* env, editor_t* eb) {
  if (sbuf_len(eb->input) <= 0) return;
  editor_start_modify(eb);
  sbuf_clear(eb->input);
  eb->pos = 0;
  edit_refresh(env,eb);
}

static void edit_delete_to_end_of_line(ic_env_t* env, editor_t* eb) { 
  ssize_t start = sbuf_find_line_start(eb->input,eb->pos);
  if (start < 0) return;
  ssize_t end = sbuf_find_line_end(eb->input,eb->pos);
  if (end < 0) return;
  editor_start_modify(eb);
  // if on an empty line, remove it completely    
  if (start == end && sbuf_char_at(eb->input,end) == '\n') {
    end++;
  }
  else if (start == end && sbuf_char_at(eb->input,start - 1) == '\n') {
    eb->pos--;
  }
  sbuf_delete_from_to( eb->input, eb->pos, end );
  edit_refresh(env,eb);
}

static void edit_delete_to_start_of_line(ic_env_t* env, editor_t* eb) {
  ssize_t start = sbuf_find_line_start(eb->input,eb->pos);
  if (start < 0) return;
  ssize_t end   = sbuf_find_line_end(eb->input,eb->pos);
  if (end < 0) return;
  editor_start_modify(eb);
  // delete start newline if it was an empty line
  bool goright = false;
  if (start > 0 && sbuf_char_at(eb->input,start-1) == '\n' && start == end) {
    // if it is an empty line remove it
    start--;
    // afterwards, move to start of next line if it exists (so the cursor stays on the same row)
    goright = true;
  }
  sbuf_delete_from_to( eb->input, start, eb->pos );
  eb->pos = start;
  if (goright) edit_cursor_right(env,eb); 
  edit_refresh(env,eb);
}

static void edit_delete_line(ic_env_t* env, editor_t* eb) {
  ssize_t start = sbuf_find_line_start(eb->input,eb->pos);
  if (start < 0) return;
  ssize_t end   = sbuf_find_line_end(eb->input,eb->pos);
  if (end < 0) return;
  editor_start_modify(eb);
  // delete newline as well so no empty line is left;
  bool goright = false;
  if (start > 0 && sbuf_char_at(eb->input,start-1) == '\n') {
    start--;
    // afterwards, move to start of next line if it exists (so the cursor stays on the same row)
    goright = true;
  }
  else if (sbuf_char_at(eb->input,end) == '\n') {
    end++;
  }
  sbuf_delete_from_to(eb->input,start,end);
  eb->pos = start;
  if (goright) edit_cursor_right(env,eb); 
  edit_refresh(env,eb);
}
 
static void edit_delete_to_start_of_word(ic_env_t* env, editor_t* eb) {
   ssize_t start = sbuf_find_word_start(eb->input,eb->pos);
  if (start < 0) return;
  editor_start_modify(eb);
  sbuf_delete_from_to( eb->input, start, eb->pos );
  eb->pos = start;
  edit_refresh(env,eb);
}

static void edit_delete_to_end_of_word(ic_env_t* env, editor_t* eb) {
  ssize_t end = sbuf_find_word_end(eb->input,eb->pos);
  if (end < 0) return;
  editor_start_modify(eb);
  sbuf_delete_from_to( eb->input, eb->pos, end );
  edit_refresh(env,eb);
}

static void edit_delete_to_start_of_ws_word(ic_env_t* env, editor_t* eb) {
  ssize_t start = sbuf_find_ws_word_start(eb->input, eb->pos);
  if (start < 0) return;
  editor_start_modify(eb);
  sbuf_delete_from_to(eb->input, start, eb->pos);
  eb->pos = start;
  edit_refresh(env, eb);
}

static void edit_delete_to_end_of_ws_word(ic_env_t* env, editor_t* eb) {
  ssize_t end = sbuf_find_ws_word_end(eb->input, eb->pos);
  if (end < 0) return;
  editor_start_modify(eb);
  sbuf_delete_from_to(eb->input, eb->pos, end);
  edit_refresh(env, eb);
}


static void edit_delete_word(ic_env_t* env, editor_t* eb) {
  ssize_t start = sbuf_find_word_start(eb->input,eb->pos);
  if (start < 0) return;
  ssize_t end   = sbuf_find_word_end(eb->input,eb->pos);
  if (end < 0) return;
  editor_start_modify(eb);  
  sbuf_delete_from_to(eb->input,start,end);
  eb->pos = start;
  edit_refresh(env,eb);
}

static void edit_swap_char( ic_env_t* env, editor_t* eb ) { 
  if (eb->pos <= 0 || eb->pos == sbuf_len(eb->input)) return;
  editor_start_modify(eb);
  eb->pos = sbuf_swap_char(eb->input,eb->pos);
  edit_refresh(env,eb);
}

static void edit_multiline_eol(ic_env_t* env, editor_t* eb) {
  if (eb->pos <= 0) return;
  if (sbuf_string(eb->input)[eb->pos-1] != env->multiline_eol) return;
  editor_start_modify(eb);
  // replace line continuation with a real newline
  sbuf_delete_at( eb->input, eb->pos-1, 1);
  sbuf_insert_at( eb->input, "\n", eb->pos-1);  
  edit_refresh(env,eb);
}

static void edit_insert_unicode(ic_env_t* env, editor_t* eb, unicode_t u) {
  editor_start_modify(eb);
  ssize_t nextpos = sbuf_insert_unicode_at(eb->input, u, eb->pos);
  if (nextpos >= 0) eb->pos = nextpos;  
  edit_refresh_hint(env, eb);
}

static void edit_auto_brace(ic_env_t* env, editor_t* eb, char c) {
  if (env->no_autobrace) return;
  const char* braces = ic_env_get_auto_braces(env);
  for (const char* b = braces; *b != 0; b += 2) {
    if (*b == c) {
      const char close = b[1];
      //if (sbuf_char_at(eb->input, eb->pos) != close) {
        sbuf_insert_char_at(eb->input, close, eb->pos);
        bool balanced = false;
        find_matching_brace(sbuf_string(eb->input), eb->pos, braces, &balanced );
        if (!balanced) {
          // don't insert if it leads to an unbalanced expression.
          sbuf_delete_char_at(eb->input, eb->pos);
        }
      //}
      return;
    }
    else if (b[1] == c) {
      // close brace, check if there we don't overwrite to the right
      if (sbuf_char_at(eb->input, eb->pos) == c) {
        sbuf_delete_char_at(eb->input, eb->pos);
      }
      return;
    }
  }
}

static void editor_auto_indent(editor_t* eb, const char* pre, const char* post ) {
  assert(eb->pos > 0 && sbuf_char_at(eb->input,eb->pos-1) == '\n');
  ssize_t prelen = ic_strlen(pre);
  if (prelen > 0) {
    if (eb->pos - 1 < prelen) return;
    if (!ic_starts_with(sbuf_string(eb->input) + eb->pos - 1 - prelen, pre)) return;
    if (!ic_starts_with(sbuf_string(eb->input) + eb->pos, post)) return;
    eb->pos = sbuf_insert_at(eb->input, "  ", eb->pos);
    sbuf_insert_char_at(eb->input, '\n', eb->pos);
  }
}

static void edit_insert_char(ic_env_t* env, editor_t* eb, char c) {
  editor_start_modify(eb);
  ssize_t nextpos = sbuf_insert_char_at( eb->input, c, eb->pos );
  if (nextpos >= 0) eb->pos = nextpos;
  edit_auto_brace(env, eb, c);
  if (c=='\n') {
    editor_auto_indent(eb, "{", "}");  // todo: custom auto indent tokens?
  }
  edit_refresh_hint(env,eb);  
}

//-------------------------------------------------------------
// Help
//-------------------------------------------------------------

#include "editline_help.c"

//-------------------------------------------------------------
// History
//-------------------------------------------------------------

#include "editline_history.c"

//-------------------------------------------------------------
// Completion
//-------------------------------------------------------------

#include "editline_completion.c"


//-------------------------------------------------------------
// Edit line: main edit loop
//-------------------------------------------------------------

static char* edit_line( ic_env_t* env, const char* prompt_text )
{
  // set up an edit buffer
  editor_t eb;
  memset(&eb, 0, sizeof(eb));
  eb.mem      = env->mem;
  eb.input    = sbuf_new(env->mem);
  eb.extra    = sbuf_new(env->mem);
  eb.hint     = sbuf_new(env->mem);
  eb.hint_help= sbuf_new(env->mem);
  eb.termw    = term_get_width(env->term);  
  eb.pos      = 0;
  eb.cur_rows = 1; 
  eb.cur_row  = 0; 
  eb.modified = false;  
  eb.prompt_text   = (prompt_text != NULL ? prompt_text : "");
  eb.history_idx   = 0;  
  editstate_init(&eb.undo);
  editstate_init(&eb.redo);
  if (eb.input==NULL || eb.extra==NULL || eb.hint==NULL || eb.hint_help==NULL) {
    return NULL;
  }

  // caching
  if (!(env->no_highlight && env->no_bracematch)) {
    eb.attrs = attrbuf_new(env->mem);
    eb.attrs_extra = attrbuf_new(env->mem);
  }
  
  // show prompt
  edit_write_prompt(env, &eb, 0, false);   

  // always a history entry for the current input
  history_push(env->history, "");

  // process keys
  code_t c;          // current key code
  while(true) {    
    // read a character
    term_flush(env->term);
    if (env->hint_delay <= 0 || sbuf_len(eb.hint) == 0) {
      // blocking read
      c = tty_read(env->tty);
    }
    else {
      // timeout to display hint
      if (!tty_read_timeout(env->tty, env->hint_delay, &c)) {
        // timed-out
        if (sbuf_len(eb.hint) > 0) {
          // display hint
          edit_refresh(env, &eb);
        }
        c = tty_read(env->tty);
      }
      else {
        // clear the pending hint if we got input before the delay expired
        sbuf_clear(eb.hint);
        sbuf_clear(eb.hint_help);
      }
    }
    
    // update terminal in case of a resize
    if (tty_term_resize_event(env->tty)) {
      edit_resize(env,&eb);            
    }

    // clear hint only after a potential resize (so resize row calculations are correct)
    const bool had_hint = (sbuf_len(eb.hint) > 0);
    sbuf_clear(eb.hint);
    sbuf_clear(eb.hint_help);

    // if the user tries to move into a hint with left-cursor or end, we complete it first
    if ((c == KEY_RIGHT || c == KEY_END) && had_hint) {
      edit_generate_completions(env, &eb, true);
      c = KEY_NONE;      
    }

    // Operations that may return
    if (c == KEY_ENTER) {
      if (!env->singleline_only && eb.pos > 0 && 
           sbuf_string(eb.input)[eb.pos-1] == env->multiline_eol && 
            edit_pos_is_at_row_end(env,&eb)) 
      {
        // replace line-continuation with newline
        edit_multiline_eol(env,&eb);        
      }
      else {
        // otherwise done
        break;
      }
    } 
    else if (c == KEY_CTRL_D) {
      if (eb.pos == 0 && editor_pos_is_at_end(&eb)) break; // ctrl+D on empty quits with NULL
      edit_delete_char(env,&eb);     // otherwise it is like delete
    } 
    else if (c == KEY_CTRL_C || c == KEY_EVENT_STOP) {
      break; // ctrl+C or STOP event quits with NULL
    }
    else if (c == KEY_ESC) {
      if (eb.pos == 0 && editor_pos_is_at_end(&eb)) break;  // ESC on empty input returns with empty input
      edit_delete_all(env,&eb);      // otherwise delete the current input
      // edit_delete_line(env,&eb);  // otherwise delete the current line
    }
    else if (c == KEY_BELL /* ^G */) {
      edit_delete_all(env,&eb);
      break; // ctrl+G cancels (and returns empty input)
    }

    // Editing Operations
    else switch(c) {
      // events
      case KEY_EVENT_RESIZE:  // not used
        edit_resize(env,&eb);
        break;
      case KEY_EVENT_AUTOTAB:
        edit_generate_completions(env, &eb, true);
        break;

      // completion, history, help, undo
      case KEY_TAB:
      case WITH_ALT('?'):
        edit_generate_completions(env,&eb,false);
        break;
      case KEY_CTRL_R:
      case KEY_CTRL_S:
        edit_history_search_with_current_word(env,&eb);
        break;
      case KEY_CTRL_P:
        edit_history_prev(env, &eb);
        break;
      case KEY_CTRL_N:
        edit_history_next(env, &eb);
        break;
      case KEY_CTRL_L:
        edit_clear_screen(env, &eb);
        break;
      case KEY_CTRL_Z:
      case WITH_CTRL('_'):
        edit_undo_restore(env, &eb);
        break;
      case KEY_CTRL_Y:
        edit_redo_restore(env, &eb);
        break;
      case KEY_F1:
        edit_show_help(env, &eb);
        break;

      // navigation
      case KEY_LEFT:
      case KEY_CTRL_B:
        edit_cursor_left(env,&eb);
        break;
      case KEY_RIGHT:
      case KEY_CTRL_F:
        if (eb.pos == sbuf_len(eb.input)) { 
          edit_generate_completions( env, &eb, false );
        }
        else {
          edit_cursor_right(env,&eb);
        }
        break;
      case KEY_UP:
        edit_cursor_row_up(env,&eb);
        break;
      case KEY_DOWN:
        edit_cursor_row_down(env,&eb);
        break;                 
      case KEY_HOME:
      case KEY_CTRL_A:
        edit_cursor_line_start(env,&eb);
        break;
      case KEY_END:
      case KEY_CTRL_E:
        edit_cursor_line_end(env,&eb);
        break;
      case KEY_CTRL_LEFT:
      case WITH_SHIFT(KEY_LEFT):    
      case WITH_ALT('b'):
        edit_cursor_prev_word(env,&eb);
        break;
      case KEY_CTRL_RIGHT:
      case WITH_SHIFT(KEY_RIGHT):      
      case WITH_ALT('f'):
        if (eb.pos == sbuf_len(eb.input)) { 
          edit_generate_completions( env, &eb, false );
        }
        else {
          edit_cursor_next_word(env,&eb);
        }
        break;      
      case KEY_CTRL_HOME:
      case WITH_SHIFT(KEY_HOME):      
      case KEY_PAGEUP:
      case WITH_ALT('<'):
        edit_cursor_to_start(env,&eb);
        break;
      case KEY_CTRL_END:
      case WITH_SHIFT(KEY_END):      
      case KEY_PAGEDOWN:
      case WITH_ALT('>'):
        edit_cursor_to_end(env,&eb);
        break;
      case WITH_ALT('m'):
        edit_cursor_match_brace(env,&eb);
        break;

      // deletion
      case KEY_BACKSP:
        edit_backspace(env,&eb);
        break;
      case KEY_DEL:
        edit_delete_char(env,&eb);
        break;
      case WITH_ALT('d'):
        edit_delete_to_end_of_word(env,&eb);
        break;
      case KEY_CTRL_W:
        edit_delete_to_start_of_ws_word(env, &eb);
        break;
      case WITH_ALT(KEY_DEL):
      case WITH_ALT(KEY_BACKSP):
        edit_delete_to_start_of_word(env,&eb);
        break;      
      case KEY_CTRL_U:
        edit_delete_to_start_of_line(env,&eb);
        break;
      case KEY_CTRL_K:
        edit_delete_to_end_of_line(env,&eb);
        break;
      case KEY_CTRL_T:
        edit_swap_char(env,&eb);
        break;

      // Editing
      case KEY_SHIFT_TAB:
      case KEY_LINEFEED: // '\n' (ctrl+J, shift+enter)
        if (!env->singleline_only) { 
          edit_insert_char(env, &eb, '\n'); 
        }
        break;
      default: {
        char chr;
        unicode_t uchr;
        if (code_is_ascii_char(c,&chr)) {
          edit_insert_char(env,&eb,chr);
        }
        else if (code_is_unicode(c, &uchr)) {
          edit_insert_unicode(env,&eb, uchr);
        }
        else {
          debug_msg( "edit: ignore code: 0x%04x\n", c);
        }
        break;
      }
    }

  }

  // goto end
  eb.pos = sbuf_len(eb.input);

  // refresh once more but without brace matching
  bool bm = env->no_bracematch;
  env->no_bracematch = true;
  edit_refresh(env,&eb);
  env->no_bracematch = bm;
  
  // save result
  char* res; 
  if ((c == KEY_CTRL_D && sbuf_len(eb.input) == 0) || c == KEY_CTRL_C || c == KEY_EVENT_STOP) {
    res = NULL;
  }
  else if (!tty_is_utf8(env->tty)) {
    res = sbuf_strdup_from_utf8(eb.input);
  }
  else {
    res = sbuf_strdup(eb.input);
  }

  // update history
  history_update(env->history, sbuf_string(eb.input));
  if (res == NULL || sbuf_len(eb.input) <= 1) { ic_history_remove_last(); } // no empty or single-char entries
  history_save(env->history);

  // free resources 
  editstate_done(env->mem, &eb.undo);
  editstate_done(env->mem, &eb.redo);
  attrbuf_free(eb.attrs);
  attrbuf_free(eb.attrs_extra);
  sbuf_free(eb.input);
  sbuf_free(eb.extra);
  sbuf_free(eb.hint);
  sbuf_free(eb.hint_help);

  return res;
}

