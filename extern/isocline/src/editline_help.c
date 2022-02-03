/* ----------------------------------------------------------------------------
  Copyright (c) 2021, Daan Leijen
  This is free software; you can redistribute it and/or modify it
  under the terms of the MIT License. A copy of the license can be
  found in the "LICENSE" file at the root of this distribution.
-----------------------------------------------------------------------------*/

//-------------------------------------------------------------
// Help: this is included into editline.c
//-------------------------------------------------------------

static const char* help[] = {
  "","Navigation:",
  "left,"
  "^b",         "go one character to the left",
  "right,"
  "^f",         "go one character to the right",
  "up",         "go one row up, or back in the history",
  "down",       "go one row down, or forward in the history",
  #ifdef __APPLE__
  "shift-left",
  #else
  "^left",
  #endif
                "go to the start of the previous word",
  #ifdef __APPLE__
  "shift-right",
  #else
  "^right",
  #endif
                "go to the end the current word",
  "home,"
  "^a",         "go to the start of the current line",
  "end,"
  "^e",         "go to the end of the current line",
  "pgup,"
  "^home",       "go to the start of the current input",
  "pgdn,"
  "^end",       "go to the end of the current input",
  "alt-m",      "jump to matching brace",
  "^p",         "go back in the history",
  "^n",         "go forward in the history",
  "^r,^s",      "search the history starting with the current word",
  "","",

  "", "Deletion:",
  "del,^d",     "delete the current character",
  "backsp,^h",  "delete the previous character",
  "^w",         "delete to preceding white space",
  "alt-backsp", "delete to the start of the current word",
  "alt-d",      "delete to the end of the current word",
  "^u",         "delete to the start of the current line",
  "^k",         "delete to the end of the current line",
  "esc",        "delete the current input, or done with empty input",
  "","",

  "", "Editing:",
  "enter",      "accept current input",
  #ifndef __APPLE__
  "^enter, ^j", "",
  "shift-tab",
  #else
  "shift-tab,^j",
  #endif
                "create a new line for multi-line input",
  //" ",          "(or type '\\' followed by enter)",
  "^l",         "clear screen",
  "^t",         "swap with previous character (move character backward)",
  "^z,^_",      "undo",
  "^y",         "redo",
  //"^C",         "done with empty input",
  //"F1",         "show this help",
  "tab",        "try to complete the current input",
  "","",
  "","In the completion menu:",
  "enter,left", "use the currently selected completion",
  "1 - 9",      "use completion N from the menu",
  "tab,down",   "select the next completion",
  "shift-tab,up","select the previous completion",
  "esc",        "exit menu without completing",
  "pgdn,^j",    "show all further possible completions",
  "","",
  "","In incremental history search:",
  "enter",      "use the currently found history entry",
  "backsp,"
  "^z",         "go back to the previous match (undo)",
  "tab,"
  "^r",         "find the next match",
  "shift-tab,"
  "^s",         "find an earlier match",
  "esc",        "exit search",
  " ","",
  NULL, NULL
};

static const char* help_initial = 
  "[ic-info]"
  "Isocline v1.0, copyright (c) 2021 Daan Leijen.\n"
  "This is free software; you can redistribute it and/or\n"
  "modify it under the terms of the MIT License.\n"
  "See <[url]https://github.com/daanx/isocline[/url]> for further information.\n"
  "We use ^<key> as a shorthand for ctrl-<key>.\n"
  "\n"
  "Overview:\n"
  "\n[ansi-lightgray]"
  "       home,ctrl-a      cursor     end,ctrl-e\n"
  "         ┌────────────────┼───────────────┐    (navigate)\n"
  //"       │                │               │\n"
  #ifndef __APPLE__
  "         │    ctrl-left   │  ctrl-right   │\n"
  #else
  "         │     alt-left   │   alt-right   │\n"
  #endif
  "         │        ┌───────┼──────┐        │    ctrl-r   : search history\n"
  "         ▼        ▼       ▼      ▼        ▼    tab      : complete word\n"
  "  prompt> [ansi-darkgray]it's the quintessential language[/]     shift-tab: insert new line\n"
  "         ▲        ▲              ▲        ▲    esc      : delete input, done\n"
  "         │        └──────────────┘        │    ctrl-z   : undo\n"
  "         │   alt-backsp        alt-d      │\n"
  //"       │                │               │\n"
  "         └────────────────────────────────┘    (delete)\n"
  "       ctrl-u                          ctrl-k\n"
  "[/ansi-lightgray][/ic-info]\n";

static void edit_show_help(ic_env_t* env, editor_t* eb) {
  edit_clear(env, eb);
  bbcode_println(env->bbcode, help_initial);
  for (ssize_t i = 0; help[i] != NULL && help[i+1] != NULL; i += 2) {
    if (help[i][0] == 0) {  
      bbcode_printf(env->bbcode, "[ic-info]%s[/]\n", help[i+1]);
    }
    else {
      bbcode_printf(env->bbcode, "  [ic-emphasis]%-13s[/][ansi-lightgray]%s%s[/]\n", help[i], (help[i+1][0] == 0 ? "" : ": "), help[i+1]);
    }
  }

  eb->cur_rows = 0;
  eb->cur_row = 0;
  edit_refresh(env, eb);
}
