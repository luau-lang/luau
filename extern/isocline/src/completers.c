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



//-------------------------------------------------------------
// Word completion 
//-------------------------------------------------------------

// free variables for word completion
typedef struct word_closure_s {
  long                  delete_before_adjust;
  void*                 prev_env;
  ic_completion_fun_t*  prev_complete;
} word_closure_t;


// word completion callback
static bool token_add_completion_ex(ic_env_t* env, void* closure, const char* replacement, const char* display, const char* help, long delete_before, long delete_after) {
  word_closure_t* wenv = (word_closure_t*)(closure);
  // call the previous completer with an adjusted delete-before
  return (*wenv->prev_complete)(env, wenv->prev_env, replacement, display, help, wenv->delete_before_adjust + delete_before, delete_after);
}


ic_public void ic_complete_word(ic_completion_env_t* cenv, const char* prefix, ic_completer_fun_t* fun,
                                    ic_is_char_class_fun_t* is_word_char) 
{
  if (is_word_char == NULL) is_word_char = &ic_char_is_nonseparator;
  
  ssize_t len = ic_strlen(prefix);
  ssize_t pos = len; // will be start of the 'word' (excluding a potential start quote)
  while (pos > 0) {
    // go back one code point
    ssize_t ofs = str_prev_ofs(prefix, pos, NULL);
    if (ofs <= 0) break;
    if (!(*is_word_char)(prefix + (pos - ofs), (long)ofs)) { 
      break;
    }
    pos -= ofs;
  }
  if (pos < 0) { pos = 0; }
  
  // stop if empty word
  // if (len == pos) return;
  
  // set up the closure
  word_closure_t wenv;
  wenv.delete_before_adjust = (long)(len - pos);
  wenv.prev_complete = cenv->complete;
  wenv.prev_env = cenv->env;
  cenv->complete = &token_add_completion_ex;
  cenv->closure = &wenv;

  // and call the user completion routine
  (*fun)(cenv, prefix + pos);

  // restore the original environment
  cenv->complete = wenv.prev_complete;
  cenv->closure = wenv.prev_env;
}


//-------------------------------------------------------------
// Quoted word completion (with escape characters)
//-------------------------------------------------------------

// free variables for word completion
typedef struct qword_closure_s {
  char         escape_char;
  char         quote;
  long         delete_before_adjust;
  stringbuf_t* sbuf;
  void*        prev_env;
  ic_is_char_class_fun_t* is_word_char;
  ic_completion_fun_t*    prev_complete;
} qword_closure_t;


// word completion callback
static bool qword_add_completion_ex(ic_env_t* env, void* closure, const char* replacement, const char* display, const char* help, 
                                       long delete_before, long delete_after) {
  qword_closure_t* wenv = (qword_closure_t*)(closure);
  sbuf_replace( wenv->sbuf, replacement );   
  if (wenv->quote != 0) {
    // add end quote
    sbuf_append_char( wenv->sbuf, wenv->quote);
  }
  else {
    // escape non-word characters if it was not quoted
    ssize_t pos = 0;
    ssize_t next;
    while ( (next = sbuf_next_ofs(wenv->sbuf, pos, NULL)) > 0 ) 
    {
      if (!(*wenv->is_word_char)(sbuf_string(wenv->sbuf) + pos, (long)next)) { // strchr(wenv->non_word_char, sbuf_char_at( wenv->sbuf, pos )) != NULL) {
        sbuf_insert_char_at( wenv->sbuf, wenv->escape_char, pos);
        pos++;
      }
      pos += next;
    }
  }
  // and call the previous completion function
  return (*wenv->prev_complete)( env, wenv->prev_env, sbuf_string(wenv->sbuf), display, help, wenv->delete_before_adjust + delete_before, delete_after );  
}


ic_public void ic_complete_qword( ic_completion_env_t* cenv, const char* prefix, ic_completer_fun_t* fun, ic_is_char_class_fun_t* is_word_char ) {
  ic_complete_qword_ex( cenv, prefix, fun, is_word_char, '\\', NULL);
}


ic_public void ic_complete_qword_ex( ic_completion_env_t* cenv, const char* prefix, ic_completer_fun_t* fun, 
                                        ic_is_char_class_fun_t* is_word_char, char escape_char, const char* quote_chars ) {
  if (is_word_char == NULL) is_word_char = &ic_char_is_nonseparator ;  
  if (quote_chars == NULL) quote_chars = "'\"";

  ssize_t len = ic_strlen(prefix);
  ssize_t pos; // will be start of the 'word' (excluding a potential start quote)
  char quote = 0;
  ssize_t quote_len = 0;
  
  // 1. look for a starting quote
  if (quote_chars[0] != 0) {
    // we go forward and count all quotes; if it is uneven, we need to complete quoted.
    ssize_t qpos_open = -1;
    ssize_t qpos_close = -1;
    ssize_t qcount = 0;
    pos = 0; 
    while(pos < len) {
      if (prefix[pos] == escape_char && prefix[pos+1] != 0 && 
           !(*is_word_char)(prefix + pos + 1, 1)) // strchr(non_word_char, prefix[pos+1]) != NULL
      {       
        pos++; // skip escape and next char
      }
      else if (qcount % 2 == 0 && strchr(quote_chars, prefix[pos]) != NULL) {
        // open quote 
        qpos_open = pos;
        quote = prefix[pos];
        qcount++;
      }
      else if (qcount % 2 == 1 && prefix[pos] == quote) {
        // close quote
        qpos_close = pos;
        qcount++;
      }
      else if (!(*is_word_char)(prefix + pos, 1)) { //  strchr(non_word_char, prefix[pos]) != NULL) {
        qpos_close = -1;
      }
      ssize_t ofs = str_next_ofs( prefix, len, pos, NULL );
      if (ofs <= 0) break;
      pos += ofs;
    }    
    if ((qcount % 2 == 0 && qpos_close >= 0) || // if the last quote is only followed by word chars, we still complete it
        (qcount % 2 == 1))                     // opening quote found
    {
      quote_len = (len - qpos_open - 1);
      pos = qpos_open + 1;  // pos points to the word start just after the quote.
    }
    else {
      quote = 0;
    }
  }

  // 2. if we did not find a quoted word, look for non-word-chars
  if (quote == 0) {
    pos = len;
    while(pos > 0) {
      // go back one code point
      ssize_t ofs = str_prev_ofs(prefix, pos, NULL );
      if (ofs <= 0) break;
      if (!(*is_word_char)(prefix + (pos - ofs), (long)ofs)) { // strchr(non_word_char, prefix[pos - ofs]) != NULL) {
        // non word char, break if it is not escaped
        if (pos <= ofs || prefix[pos - ofs - 1] != escape_char) break; 
        // otherwise go on
        pos--; // skip escaped char
      }
      pos -= ofs;
    }
  }

  // stop if empty word
  // if (len == pos) return;

  // allocate new unescaped word prefix
  char* word = mem_strndup( cenv->env->mem, prefix + pos, (quote==0 ? len - pos : quote_len));
  if (word == NULL) return;

  if (quote == 0) {
    // unescape prefix
    ssize_t wlen = len - pos;
    ssize_t wpos = 0;
    while (wpos < wlen) {
      ssize_t ofs = str_next_ofs(word, wlen, wpos, NULL);
      if (ofs <= 0) break;
      if (word[wpos] == escape_char && word[wpos+1] != 0 &&
           !(*is_word_char)(word + wpos + 1, (long)ofs)) // strchr(non_word_char, word[wpos+1]) != NULL) {
      {
        ic_memmove(word + wpos, word + wpos + 1, wlen - wpos /* including 0 */);
      }
      wpos += ofs;
    }
  }
  #ifdef _WIN32
  else {
    // remove inner quote: "c:\Program Files\"Win
    ssize_t wlen = len - pos;
    ssize_t wpos = 0;
    while (wpos < wlen) {
      ssize_t ofs = str_next_ofs(word, wlen, wpos, NULL);
      if (ofs <= 0) break;
      if (word[wpos] == escape_char && word[wpos+1] == quote) {
        word[wpos+1] = escape_char;
        ic_memmove(word + wpos, word + wpos + 1, wlen - wpos /* including 0 */);
      }
      wpos += ofs;
    }
  }
  #endif

  // set up the closure
  qword_closure_t wenv;
  wenv.quote          = quote;
  wenv.is_word_char   = is_word_char;
  wenv.escape_char    = escape_char;
  wenv.delete_before_adjust = (long)(len - pos);
  wenv.prev_complete  = cenv->complete;
  wenv.prev_env       =  cenv->env;
  wenv.sbuf = sbuf_new(cenv->env->mem);
  if (wenv.sbuf == NULL) { mem_free(cenv->env->mem, word); return; }
  cenv->complete = &qword_add_completion_ex;
  cenv->closure = &wenv;

  // and call the user completion routine
  (*fun)( cenv, word );

  // restore the original environment
  cenv->complete = wenv.prev_complete;
  cenv->closure = wenv.prev_env;

  sbuf_free(wenv.sbuf);
  mem_free(cenv->env->mem, word);  
}




//-------------------------------------------------------------
// Complete file names
// Listing files
//-------------------------------------------------------------
#include <stdlib.h>

typedef enum file_type_e {
  // must follow BSD style LSCOLORS order
  FT_DEFAULT = 0,
  FT_DIR,
  FT_SYM,
  FT_SOCK,
  FT_PIPE,
  FT_BLOCK,
  FT_CHAR,
  FT_SETUID,
  FT_SETGID,
  FT_DIR_OW_STICKY,
  FT_DIR_OW,
  FT_DIR_STICKY,
  FT_EXE,
  FT_LAST
} file_type_t;

static int         cli_color; // 1 enabled, 0 not initialized, -1 disabled
static const char* lscolors  = "exfxcxdxbxegedabagacad";  // default BSD setting
static const char* ls_colors;
static const char* ls_colors_names[] = { "no=","di=","ln=","so=","pi=","bd=","cd=","su=","sg=","tw=","ow=","st=","ex=", NULL };

static bool ls_colors_init(void) {
  if (cli_color != 0) return (cli_color >= 1);
  // colors enabled?
  const char* s = getenv("CLICOLOR");
  if (s==NULL || (strcmp(s, "1")!=0 && strcmp(s, "") != 0)) {
    cli_color = -1;
    return false;
  }
  cli_color = 1;
  s = getenv("LS_COLORS");
  if (s != NULL) { ls_colors = s;  }
  s = getenv("LSCOLORS");
  if (s != NULL) { lscolors = s; }  
  return true;
}

static bool ls_valid_esc(ssize_t c) {
  return ((c==0 || c==1 || c==4 || c==7 || c==22 || c==24  || c==27) ||
    (c >= 30 && c <= 37) || (c >= 40 && c <= 47) ||
    (c >= 90 && c <= 97) || (c >= 100 && c <= 107));
}

static bool ls_colors_from_key(stringbuf_t* sb, const char* key) {
  // find key
  ssize_t keylen = ic_strlen(key);
  if (keylen <= 0) return false;
  const char* p = strstr(ls_colors, key);
  if (p == NULL) return false;
  p += keylen;
  if (key[keylen-1] != '=') {
    if (*p != '=') return false;
    p++;
  }
  ssize_t len = 0;
  while (p[len] != 0 && p[len] != ':') {
    len++;
  }
  if (len <= 0) return false;
  sbuf_append(sb, "[ansi-sgr=\"" );
  sbuf_append_n(sb, p, len );
  sbuf_append(sb, "\"]");
  return true;
}

static int ls_colors_from_char(char c) {
  if (c >= 'a' && c <= 'h')      { return (c - 'a'); }
  else if (c >= 'A' && c <= 'H') { return (c - 'A') + 8; }
  else if (c == 'x')             { return 256; }
  else return 256; // default
}

static bool ls_colors_append(stringbuf_t* sb, file_type_t ft, const char* ext) {
  if (!ls_colors_init()) return false;
  if (ls_colors != NULL) {
    // GNU style
    if (ft == FT_DEFAULT && ext != NULL) {
      // first try extension match
      if (ls_colors_from_key(sb, ext)) return true;
    }
    if (ft >= FT_DEFAULT && ft < FT_LAST) {
      // then a filetype match
      const char* key = ls_colors_names[ft];
      if (ls_colors_from_key(sb, key)) return true;
    }    
  }
  else if (lscolors != NULL) {
    // BSD style
    char fg = 'x';
    char bg = 'x';
    if (ic_strlen(lscolors) > (2*(ssize_t)ft)+1) {
      fg = lscolors[2*ft];
      bg = lscolors[2*ft + 1];
    }
    sbuf_appendf(sb, "[ansi-color=%d ansi-bgcolor=%d]", ls_colors_from_char(fg), ls_colors_from_char(bg) );
    return true;
  }
  return false;
}

static void ls_colorize(bool no_lscolor, stringbuf_t* sb, file_type_t ft, const char* name, const char* ext, char dirsep) {
  bool close = (no_lscolor ? false : ls_colors_append( sb, ft, ext));
  sbuf_append(sb, "[!pre]" );
  sbuf_append(sb, name);
  if (dirsep != 0) sbuf_append_char(sb, dirsep);
  sbuf_append(sb,"[/pre]" );
  if (close) { sbuf_append(sb, "[/]"); }
}

#if defined(_WIN32)
#include <io.h>
#include <sys/stat.h>

static bool os_is_dir(const char* cpath) {
  struct _stat64 st = { 0 };
  _stat64(cpath, &st);
  return ((st.st_mode & _S_IFDIR) != 0);
}

static file_type_t os_get_filetype(const char* cpath) {
  struct _stat64 st = { 0 };
  _stat64(cpath, &st);
  if (((st.st_mode) & _S_IFDIR) != 0) return FT_DIR;
  if (((st.st_mode) & _S_IFCHR) != 0) return FT_CHAR;
  if (((st.st_mode) & _S_IFIFO) != 0) return FT_PIPE;
  if (((st.st_mode) & _S_IEXEC) != 0) return FT_EXE;
  return FT_DEFAULT;
}


#define dir_cursor intptr_t
#define dir_entry  struct __finddata64_t

static bool os_findfirst(alloc_t* mem, const char* path, dir_cursor* d, dir_entry* entry) {
  stringbuf_t* spath = sbuf_new(mem);
  if (spath == NULL) return false;
  sbuf_append(spath, path);
  sbuf_append(spath, "\\*");
  *d = _findfirsti64(sbuf_string(spath), entry);
  mem_free(mem,spath);
  return (*d != -1);
}

static bool os_findnext(dir_cursor d, dir_entry* entry) {
  return (_findnexti64(d, entry) == 0);  
}

static void os_findclose(dir_cursor d) {
  _findclose(d);
}

static const char* os_direntry_name(dir_entry* entry) {
  return entry->name;  
}

static bool os_path_is_absolute( const char* path ) {
  if (path != NULL && path[0] != 0 && path[1] == ':' && (path[2] == '\\' || path[2] == '/' || path[2] == 0)) {
    char drive = path[0];
    return ((drive >= 'A' && drive <= 'Z') || (drive >= 'a' && drive <= 'z'));
  }
  else return false;
}

ic_private char ic_dirsep(void) {
  return '\\';
}
#else

#include <sys/types.h>
#include <sys/stat.h>
#include <dirent.h>
#include <errno.h>

static bool os_is_dir(const char* cpath) {
  struct stat st;
  memset(&st, 0, sizeof(st));
  stat(cpath, &st);
  return (S_ISDIR(st.st_mode));
}

static file_type_t os_get_filetype(const char* cpath) {
  struct stat st;
  memset(&st, 0, sizeof(st));
  lstat(cpath, &st);
  switch ((st.st_mode)&S_IFMT) {
    case S_IFSOCK: return FT_SOCK;
    case S_IFLNK: {
      return FT_SYM;
    }
    case S_IFIFO:  return FT_PIPE;
    case S_IFCHR:  return FT_CHAR;
    case S_IFBLK:  return FT_BLOCK;
    case S_IFDIR: {
      if ((st.st_mode & S_ISUID) != 0) return FT_SETUID;
      if ((st.st_mode & S_ISGID) != 0) return FT_SETGID;
      if ((st.st_mode & S_IWGRP) != 0 && (st.st_mode & S_ISVTX) != 0) return FT_DIR_OW_STICKY;
      if ((st.st_mode & S_IWGRP)) return FT_DIR_OW;
      if ((st.st_mode & S_ISVTX)) return FT_DIR_STICKY;
      return FT_DIR;
    }
    case S_IFREG:
    default: {
      if ((st.st_mode & S_IXUSR) != 0) return FT_EXE;
      return FT_DEFAULT;
    }
  }  
}


#define dir_cursor DIR*
#define dir_entry  struct dirent*

static bool os_findnext(dir_cursor d, dir_entry* entry) {
  *entry = readdir(d);
  return (*entry != NULL);
}

static bool os_findfirst(alloc_t* mem, const char* cpath, dir_cursor* d, dir_entry* entry) {
  ic_unused(mem);
  *d = opendir(cpath);
  if (*d == NULL) {
    return false;
  }
  else {
    return os_findnext(*d, entry);
  }
}

static void os_findclose(dir_cursor d) {
  closedir(d);
}

static const char* os_direntry_name(dir_entry* entry) {
  return (*entry)->d_name;  
}

static bool os_path_is_absolute( const char* path ) {
  return (path != NULL && path[0] == '/');
}

ic_private char ic_dirsep(void) {
  return '/';
}
#endif



//-------------------------------------------------------------
// File completion 
//-------------------------------------------------------------

static bool ends_with_n(const char* name, ssize_t name_len, const char* ending, ssize_t len) {
  if (name_len < len) return false;
  if (ending == NULL || len <= 0) return true;
  for (ssize_t i = 1; i <= len; i++) {
    char c1 = name[name_len - i];
    char c2 = ending[len - i];
    #ifdef _WIN32
    if (ic_tolower(c1) != ic_tolower(c2)) return false;
    #else
    if (c1 != c2) return false;
    #endif
  }
  return true;
}

static bool match_extension(const char* name, const char* extensions) {
  if (extensions == NULL || extensions[0] == 0) return true;
  if (name == NULL) return false;
  ssize_t name_len = ic_strlen(name);
  ssize_t len = ic_strlen(extensions);
  ssize_t cur = 0;  
  //debug_msg("match extensions: %s ~ %s", name, extensions);
  for (ssize_t end = 0; end <= len; end++) {
    if (extensions[end] == ';' || extensions[end] == 0) {
      if (ends_with_n(name, name_len, extensions+cur, (end - cur))) {
        return true;
      }
      cur = end+1;
    }
  }
  return false;
}

static bool filename_complete_indir( ic_completion_env_t* cenv, stringbuf_t* dir, 
                                      stringbuf_t* dir_prefix, stringbuf_t* display,
                                       const char* base_prefix, 
                                        char dir_sep, const char* extensions ) 
{
  dir_cursor d = 0;
  dir_entry entry;
  bool cont = true;
  if (os_findfirst(cenv->env->mem, sbuf_string(dir), &d, &entry)) {
    do {
      const char* name = os_direntry_name(&entry);
      if (name != NULL && strcmp(name, ".") != 0 && strcmp(name, "..") != 0 && 
          ic_istarts_with(name, base_prefix))
      {
        // possible match, first check if it is a directory
        file_type_t ft;
        bool isdir;
        const ssize_t plen = sbuf_len(dir_prefix);
        sbuf_append(dir_prefix, name);
        { // check directory and potentially add a dirsep to the dir_prefix
          const ssize_t dlen = sbuf_len(dir);
          sbuf_append_char(dir,ic_dirsep());
          sbuf_append(dir,name);
          ft = os_get_filetype(sbuf_string(dir));
          isdir = os_is_dir(sbuf_string(dir));
          if (isdir && dir_sep != 0) {
            sbuf_append_char(dir_prefix,dir_sep); 
          }
          sbuf_delete_from(dir,dlen);  // restore dir
        }
        if (isdir || match_extension(name, extensions)) {
          // add completion
          sbuf_clear(display);
          ls_colorize(cenv->env->no_lscolors, display, ft, name, NULL, (isdir ? dir_sep : 0));
          cont = ic_add_completion_ex(cenv, sbuf_string(dir_prefix), sbuf_string(display), NULL);
        }
        sbuf_delete_from( dir_prefix, plen ); // restore dir_prefix
      }
    } while (cont && os_findnext(d, &entry));
    os_findclose(d);
  }
  return cont;
}

typedef struct filename_closure_s {
  const char* roots;
  const char* extensions;
  char        dir_sep;
} filename_closure_t;

static void filename_completer( ic_completion_env_t* cenv, const char* prefix ) {
  if (prefix == NULL) return;
  filename_closure_t* fclosure = (filename_closure_t*)cenv->arg;  
  stringbuf_t* root_dir   = sbuf_new(cenv->env->mem);
  stringbuf_t* dir_prefix = sbuf_new(cenv->env->mem);
  stringbuf_t* display    = sbuf_new(cenv->env->mem);  
  if (root_dir!=NULL && dir_prefix != NULL && display != NULL) 
  {
    // split prefix in dir_prefix / base.
    const char* base = strrchr(prefix,'/');
    #ifdef _WIN32
    const char* base2 = strrchr(prefix,'\\');
    if (base == NULL || base2 > base) base = base2;
    #endif
    if (base != NULL) {
      base++; 
      sbuf_append_n(dir_prefix, prefix, base - prefix ); // includes dir separator
    }

    // absolute path
    if (os_path_is_absolute(prefix)) {
      // do not use roots but try to complete directly
      if (base != NULL) {
        sbuf_append_n( root_dir, prefix, (base - prefix));  // include dir separator
      }
      filename_complete_indir( cenv, root_dir, dir_prefix, display,  
                                (base != NULL ? base : prefix), 
                                 fclosure->dir_sep, fclosure->extensions );   
    }
    else {
      // relative path, complete with respect to every root.
      const char* next;
      const char* root = fclosure->roots;
      while ( root != NULL ) {
        // create full root in `root_dir`
        sbuf_clear(root_dir);
        next = strchr(root,';');
        if (next == NULL) {
          sbuf_append( root_dir, root );
          root = NULL;
        }
        else {
          sbuf_append_n( root_dir, root, next - root );
          root = next + 1;
        }      
        sbuf_append_char( root_dir, ic_dirsep());
          
        // add the dir_prefix to the root
        if (base != NULL) {
          sbuf_append_n( root_dir, prefix, (base - prefix) - 1);
        }

        // and complete in this directory    
        filename_complete_indir( cenv, root_dir, dir_prefix, display,
                                  (base != NULL ? base : prefix), 
                                   fclosure->dir_sep, fclosure->extensions);
      }
    }
  }
  sbuf_free(display);
  sbuf_free(root_dir);
  sbuf_free(dir_prefix);
}

ic_public void ic_complete_filename( ic_completion_env_t* cenv, const char* prefix, char dir_sep, const char* roots, const char* extensions ) {
  if (roots == NULL) roots = ".";
  if (extensions == NULL) extensions = "";
  if (dir_sep == 0) dir_sep = ic_dirsep();
  filename_closure_t fclosure;
  fclosure.dir_sep = dir_sep;
  fclosure.roots = roots; 
  fclosure.extensions = extensions;
  cenv->arg = &fclosure;
  ic_complete_qword_ex( cenv, prefix, &filename_completer, &ic_char_is_filename_letter, '\\', "'\"");  
}
