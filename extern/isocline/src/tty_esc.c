/* ----------------------------------------------------------------------------
  Copyright (c) 2021, Daan Leijen
  This is free software; you can redistribute it and/or modify it
  under the terms of the MIT License. A copy of the license can be
  found in the "LICENSE" file at the root of this distribution.
-----------------------------------------------------------------------------*/
#include <string.h>
#include "tty.h"

/*-------------------------------------------------------------
Decoding escape sequences to key codes.
This is a bit tricky there are many variants to encode keys as escape sequences, see for example:
- <http://www.leonerd.org.uk/hacks/fixterms/>.
- <https://en.wikipedia.org/wiki/ANSI_escape_code#CSI_(Control_Sequence_Introducer)_sequences>
- <https://www.xfree86.org/current/ctlseqs.html>
- <https://vt100.net/docs/vt220-rm/contents.html>
- <https://www.ecma-international.org/wp-content/uploads/ECMA-48_5th_edition_june_1991.pdf>

Generally, for our purposes we accept a subset of escape sequences as:

  escseq ::= ESC 
          |  ESC char
          |  ESC start special? (number (';' modifiers)?)? final

where:
  char         ::= [\x00-\xFF]               # any character
  special      ::= [:<=>?]             
  number       ::= [0-9+]
  modifiers    ::= [1-9]      
  intermediate ::= [\x20-\x2F]               # !"#$%&'()*+,-./
  final        ::= [\x40-\x7F]               # @A–Z[\]^_`a–z{|}~
  ESC          ::= '\x1B'
  CSI          ::= ESC '['
  SS3          ::= ESC 'O'

In ECMA48 `special? (number (';' modifiers)?)?` is the more liberal `[\x30-\x3F]*` 
but that seems never used for key codes. If the number (vtcode or unicode) or the 
modifiers are not given, we assume these are '1'. 
We then accept the following key sequences:

  key ::= ESC                                              # lone ESC
       |  ESC char                                         # Alt+char
       |  ESC '[' special? vtcode  ';' modifiers '~'       # vt100 codes
       |  ESC '[' special? '1'     ';' modifiers [A-Z]     # xterm codes
       |  ESC 'O' special? '1'     ';' modifiers [A-Za-z]  # SS3 codes
       |  ESC '[' special? unicode ';' modifiers 'u'       # direct unicode code

Moreover, we translate the following special cases that do not fit into the above grammar.
First we translate away special starter sequences:
---------------------------------------------------------------------
  ESC '[' '[' ..      ~>  ESC '[' ..                  # Linux sometimes uses extra '[' for CSI
  ESC '[' 'O' ..      ~>  ESC 'O' ..                  # Linux sometimes uses extra '[' for SS3
  ESC 'o' ..          ~>  ESC 'O' ..                  # Eterm: ctrl + SS3
  ESC '?' ..          ~>  ESC 'O' ..                  # vt52 treated as SS3

And then translate the following special cases into a standard form:
---------------------------------------------------------------------
  ESC '[' .. '@'      ~>  ESC '[' '3' '~'             # Del on Mach
  ESC '[' .. '9'      ~>  ESC '[' '2' '~'             # Ins on Mach
  ESC .. [^@$]        ~>  ESC .. '~'                  # ETerm,xrvt,urxt: ^ = ctrl, $ = shift, @ = alt
  ESC '[' [a-d]       ~>  ESC '[' '1' ';' '2' [A-D]   # Eterm shift+<cursor>
  ESC 'O' [1-9] final ~>  ESC 'O' '1' ';' [1-9] final # modifiers as parameter 1 (like on Haiku)
  ESC '[' [1-9] [^~u] ~>  ESC 'O' '1' ';' [1-9] final # modifiers as parameter 1

The modifier keys are encoded as "(modifiers-1) & mask" where the 
shift mask is 0x01, alt 0x02 and ctrl 0x04. Therefore:
------------------------------------------------------------
  1:  -           5: ctrl            9: alt  (for minicom)
  2:  shift       6: shift+ctrl
  3:  alt         7: alt+ctrl
  4:  shift+alt   8: shift+alt+ctrl

The different encodings fox vt100, xterm, and SS3 are:

vt100:  ESC [ vtcode ';' modifiers '~'
--------------------------------------
  1:  Home       10-15: F1-F5
  2:  Ins        16   : F5
  3:  Del        17-21: F6-F10
  4:  End        23-26: F11-F14
  5:  PageUp     28   : F15
  6:  PageDn     29   : F16
  7:  Home       31-34: F17-F20
  8:  End

xterm: ESC [ 1 ';' modifiers [A-Z]
-----------------------------------
  A: Up          N: F2        
  B: Down        O: F3       
  C: Right       P: F4       
  D: Left        Q: F5       
  E: '5'         R: F6       
  F: End         S: F7       
  G:             T: F8       
  H: Home        U: PageDn       
  I: PageUp      V: PageUp       
  J:             W: F11      
  K:             X: F12      
  L: Ins         Y: End      
  M: F1          Z: shift+Tab

SS3:   ESC 'O' 1 ';' modifiers [A-Za-z]
---------------------------------------
  (normal)                       (numpad)
  A: Up          N:              a: Up        n:           
  B: Down        O:              b: Down      o: 
  C: Right       P: F1           c: Right     p: Ins  
  D: Left        Q: F2           d: Left      q: End  
  E: '5'         R: F3           e:           r: Down 
  F: End         S: F4           f:           s: PageDn
  G:             T: F5           g:           t: Left 
  H: Home        U: F6           h:           u: '5'
  I: Tab         V: F7           i:           v: Right
  J:             W: F8           j: '*'       w: Home 
  K:             X: F9           k: '+'       x: Up 
  L:             Y: F10          l: ','       y: PageUp 
  M: \x0A '\n'   Z: shift+Tab    m: '-'       z:  
    
-------------------------------------------------------------*/

//-------------------------------------------------------------
// Decode escape sequences
//-------------------------------------------------------------

static code_t esc_decode_vt(uint32_t vt_code ) {
  switch(vt_code) {
    case 1: return KEY_HOME; 
    case 2: return KEY_INS;
    case 3: return KEY_DEL;
    case 4: return KEY_END;          
    case 5: return KEY_PAGEUP;
    case 6: return KEY_PAGEDOWN;
    case 7: return KEY_HOME;
    case 8: return KEY_END;          
    default: 
      if (vt_code >= 10 && vt_code <= 15) return KEY_F(1  + (vt_code - 10));
      if (vt_code == 16) return KEY_F5; // minicom
      if (vt_code >= 17 && vt_code <= 21) return KEY_F(6  + (vt_code - 17));
      if (vt_code >= 23 && vt_code <= 26) return KEY_F(11 + (vt_code - 23));
      if (vt_code >= 28 && vt_code <= 29) return KEY_F(15 + (vt_code - 28));
      if (vt_code >= 31 && vt_code <= 34) return KEY_F(17 + (vt_code - 31));
  }
  return KEY_NONE;
}

static code_t esc_decode_xterm( uint8_t xcode ) {
  // ESC [
  switch(xcode) {
    case 'A': return KEY_UP;
    case 'B': return KEY_DOWN;
    case 'C': return KEY_RIGHT;
    case 'D': return KEY_LEFT;
    case 'E': return '5';          // numpad 5
    case 'F': return KEY_END;
    case 'H': return KEY_HOME;
    case 'Z': return KEY_TAB | KEY_MOD_SHIFT;
    // Freebsd:
    case 'I': return KEY_PAGEUP;  
    case 'L': return KEY_INS;   
    case 'M': return KEY_F1;
    case 'N': return KEY_F2;
    case 'O': return KEY_F3;
    case 'P': return KEY_F4;       // note: differs from <https://en.wikipedia.org/wiki/ANSI_escape_code#CSI_(Control_Sequence_Introducer)_sequences>
    case 'Q': return KEY_F5;
    case 'R': return KEY_F6;
    case 'S': return KEY_F7;
    case 'T': return KEY_F8;
    case 'U': return KEY_PAGEDOWN; // Mach
    case 'V': return KEY_PAGEUP;   // Mach
    case 'W': return KEY_F11;
    case 'X': return KEY_F12;    
    case 'Y': return KEY_END;      // Mach       
  }
  return KEY_NONE;
}

static code_t esc_decode_ss3( uint8_t ss3_code ) {
  // ESC O 
  switch(ss3_code) {
    case 'A': return KEY_UP;
    case 'B': return KEY_DOWN;
    case 'C': return KEY_RIGHT;
    case 'D': return KEY_LEFT;
    case 'E': return '5';           // numpad 5
    case 'F': return KEY_END;
    case 'H': return KEY_HOME;
    case 'I': return KEY_TAB;
    case 'Z': return KEY_TAB | KEY_MOD_SHIFT;
    case 'M': return KEY_LINEFEED; 
    case 'P': return KEY_F1;
    case 'Q': return KEY_F2;
    case 'R': return KEY_F3;
    case 'S': return KEY_F4;
    // on Mach
    case 'T': return KEY_F5;
    case 'U': return KEY_F6;
    case 'V': return KEY_F7;
    case 'W': return KEY_F8;
    case 'X': return KEY_F9;  // '=' on vt220
    case 'Y': return KEY_F10;
    // numpad
    case 'a': return KEY_UP;
    case 'b': return KEY_DOWN;
    case 'c': return KEY_RIGHT;
    case 'd': return KEY_LEFT;
    case 'j': return '*';
    case 'k': return '+';
    case 'l': return ',';
    case 'm': return '-'; 
    case 'n': return KEY_DEL; // '.'
    case 'o': return '/'; 
    case 'p': return KEY_INS;
    case 'q': return KEY_END;  
    case 'r': return KEY_DOWN; 
    case 's': return KEY_PAGEDOWN; 
    case 't': return KEY_LEFT; 
    case 'u': return '5';
    case 'v': return KEY_RIGHT;
    case 'w': return KEY_HOME;  
    case 'x': return KEY_UP; 
    case 'y': return KEY_PAGEUP;   
  }
  return KEY_NONE;
}

static void tty_read_csi_num(tty_t* tty, uint8_t* ppeek, uint32_t* num, long esc_timeout) {
  *num = 1; // default
  ssize_t count = 0;
  uint32_t i = 0;
  while (*ppeek >= '0' && *ppeek <= '9' && count < 16) {    
    uint8_t digit = *ppeek - '0';
    if (!tty_readc_noblock(tty,ppeek,esc_timeout)) break;  // peek is not modified in this case 
    count++;
    i = 10*i + digit; 
  }
  if (count > 0) *num = i;
}

static code_t tty_read_csi(tty_t* tty, uint8_t c1, uint8_t peek, code_t mods0, long esc_timeout) {
  // CSI starts with 0x9b (c1=='[') | ESC [ (c1=='[') | ESC [Oo?] (c1 == 'O')  /* = SS3 */
  
  // check for extra starter '[' (Linux sends ESC [ [ 15 ~  for F5 for example)
  if (c1 == '[' && strchr("[Oo", (char)peek) != NULL) {
    uint8_t cx = peek;
    if (tty_readc_noblock(tty,&peek,esc_timeout)) {
      c1 = cx;
    }
  }

  // "special" characters ('?' is used for private sequences)
  uint8_t special = 0;
  if (strchr(":<=>?",(char)peek) != NULL) { 
    special = peek;
    if (!tty_readc_noblock(tty,&peek,esc_timeout)) {  
      tty_cpush_char(tty,special); // recover
      return (key_unicode(c1) | KEY_MOD_ALT);       // Alt+<anychar>
    }
  }

  // up to 2 parameters that default to 1
  uint32_t num1 = 1;
  uint32_t num2 = 1;
  tty_read_csi_num(tty,&peek,&num1,esc_timeout);
  if (peek == ';') {
    if (!tty_readc_noblock(tty,&peek,esc_timeout)) return KEY_NONE;
    tty_read_csi_num(tty,&peek,&num2,esc_timeout);
  }

  // the final character (we do not allow 'intermediate characters')
  uint8_t final = peek;
  code_t  modifiers = mods0;

  debug_msg("tty: escape sequence: ESC %c %c %d;%d %c\n", c1, (special == 0 ? '_' : special), num1, num2, final);
  
  // Adjust special cases into standard ones.
  if ((final == '@' || final == '9') && c1 == '[' && num1 == 1) {
    // ESC [ @, ESC [ 9  : on Mach
    if (final == '@')      num1 = 3; // DEL
    else if (final == '9') num1 = 2; // INS 
    final = '~';
  }
  else if (final == '^' || final == '$' || final == '@') {  
    // Eterm/rxvt/urxt  
    if (final=='^') modifiers |= KEY_MOD_CTRL;
    if (final=='$') modifiers |= KEY_MOD_SHIFT;
    if (final=='@') modifiers |= KEY_MOD_SHIFT | KEY_MOD_CTRL;
    final = '~';
  }
  else if (c1 == '[' && final >= 'a' && final <= 'd') {  // note: do not catch ESC [ .. u  (for unicode)
    // ESC [ [a-d]  : on Eterm for shift+ cursor
    modifiers |= KEY_MOD_SHIFT;
    final = 'A' + (final - 'a');
  }
  
  if (((c1 == 'O') || (c1=='[' && final != '~' && final != 'u')) &&
      (num2 == 1 && num1 > 1 && num1 <= 8)) 
  {
    // on haiku the modifier can be parameter 1, make it parameter 2 instead
    num2 = num1;
    num1 = 1;
  }

  // parameter 2 determines the modifiers
  if (num2 > 1 && num2 <= 9) {
    if (num2 == 9) num2 = 3; // iTerm2 in xterm mode
    num2--;
    if (num2 & 0x1) modifiers |= KEY_MOD_SHIFT;
    if (num2 & 0x2) modifiers |= KEY_MOD_ALT;
    if (num2 & 0x4) modifiers |= KEY_MOD_CTRL;
  }

  // and translate
  code_t code = KEY_NONE;
  if (final == '~') {
    // vt codes
    code = esc_decode_vt(num1);
  }
  else if (c1 == '[' && final == 'u') {
    // unicode
    code = key_unicode(num1);
  }
  else if (c1 == 'O' && ((final >= 'A' && final <= 'Z') || (final >= 'a' && final <= 'z'))) {
    // ss3
    code = esc_decode_ss3(final);
  }
  else if (num1 == 1 && final >= 'A' && final <= 'Z') {
    // xterm 
    code = esc_decode_xterm(final);
  }
  else if (c1 == '[' && final == 'R') {
    // cursor position
    code = KEY_NONE;
  }  

  if (code == KEY_NONE && final != 'R') { 
    debug_msg("tty: ignore escape sequence: ESC %c %zu;%zu %c\n", c1, num1, num2, final); 
  }
  return (code != KEY_NONE ? (code | modifiers) : KEY_NONE);
}

static code_t tty_read_osc( tty_t* tty, uint8_t* ppeek, long esc_timeout ) {
  debug_msg("discard OSC response..\n");
  // keep reading until termination: OSC is terminated by BELL, or ESC \ (ST)  (and STX)
  while (true) {
    uint8_t c = *ppeek;
    if (c <= '\x07') {  // BELL and anything below (STX, ^C, ^D)
      if (c != '\x07') { tty_cpush_char( tty, c ); }
      break;
    }
    else if (c=='\x1B') {
      uint8_t c1;
      if (!tty_readc_noblock(tty, &c1, esc_timeout)) break;
      if (c1=='\\') break;
      tty_cpush_char(tty,c1);
    }
    if (!tty_readc_noblock(tty, ppeek, esc_timeout)) break;
  }
  return KEY_NONE;
}

ic_private code_t tty_read_esc(tty_t* tty, long esc_initial_timeout, long esc_timeout) {
  code_t  mods = 0;
  uint8_t peek = 0;
  
  // lone ESC?
  if (!tty_readc_noblock(tty, &peek, esc_initial_timeout)) return KEY_ESC;

  // treat ESC ESC as Alt modifier (macOS sends ESC ESC [ [A-D] for alt-<cursor>)
  if (peek == KEY_ESC) {
    if (!tty_readc_noblock(tty, &peek, esc_timeout)) goto alt;
    mods |= KEY_MOD_ALT;
  }

  // CSI ?
  if (peek == '[') {
    if (!tty_readc_noblock(tty, &peek, esc_timeout)) goto alt;
    return tty_read_csi(tty, '[', peek, mods, esc_timeout);  // ESC [ ...
  }

  // SS3?
  if (peek == 'O' || peek == 'o' || peek == '?' /*vt52*/) {
    uint8_t c1 = peek;
    if (!tty_readc_noblock(tty, &peek, esc_timeout)) goto alt;
    if (c1 == 'o') { 
      // ETerm uses this for ctrl+<cursor>
      mods |= KEY_MOD_CTRL;
    }
    // treat all as standard SS3 'O'
    return tty_read_csi(tty,'O',peek,mods, esc_timeout);  // ESC [Oo?] ...
  }

  // OSC: we may get a delayed query response; ensure it is ignored
  if (peek == ']') {
    if (!tty_readc_noblock(tty, &peek, esc_timeout)) goto alt;
    return tty_read_osc(tty, &peek, esc_timeout);  // ESC ] ...
  }

alt:  
  // Alt+<char>
  return (key_unicode(peek) | KEY_MOD_ALT);  // ESC <anychar>
}
