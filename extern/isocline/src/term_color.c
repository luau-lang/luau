/* ----------------------------------------------------------------------------
  Copyright (c) 2021, Daan Leijen
  This is free software; you can redistribute it and/or modify it
  under the terms of the MIT License. A copy of the license can be
  found in the "LICENSE" file at the root of this distribution.
-----------------------------------------------------------------------------*/

// This file is included in "term.c"

//-------------------------------------------------------------
// Standard ANSI palette for 256 colors
//-------------------------------------------------------------

static uint32_t ansi256[256] = {   
  // not const as on some platforms (e.g. Windows, xterm) we update the first 16 entries with the actual used colors.
  // 0, standard ANSI
  0x000000, 0x800000, 0x008000, 0x808000, 0x000080, 0x800080, 
  0x008080, 0xc0c0c0,
  // 8, bright ANSI
  0x808080, 0xff0000, 0x00ff00, 0xffff00, 0x0000ff, 0xff00ff, 
  0x00ffff, 0xffffff,
  // 6x6x6 RGB colors
  // 16
  0x000000, 0x00005f, 0x000087, 0x0000af, 0x0000d7, 0x0000ff,
  0x005f00, 0x005f5f, 0x005f87, 0x005faf, 0x005fd7, 0x005fff,
  0x008700, 0x00875f, 0x008787, 0x0087af, 0x0087d7, 0x0087ff,
  0x00af00, 0x00af5f, 0x00af87, 0x00afaf, 0x00afd7, 0x00afff,
  0x00d700, 0x00d75f, 0x00d787, 0x00d7af, 0x00d7d7, 0x00d7ff,
  0x00ff00, 0x00ff5f, 0x00ff87, 0x00ffaf, 0x00ffd7, 0x00ffff,
  // 52
  0x5f0000, 0x5f005f, 0x5f0087, 0x5f00af, 0x5f00d7, 0x5f00ff,
  0x5f5f00, 0x5f5f5f, 0x5f5f87, 0x5f5faf, 0x5f5fd7, 0x5f5fff,
  0x5f8700, 0x5f875f, 0x5f8787, 0x5f87af, 0x5f87d7, 0x5f87ff,
  0x5faf00, 0x5faf5f, 0x5faf87, 0x5fafaf, 0x5fafd7, 0x5fafff,
  0x5fd700, 0x5fd75f, 0x5fd787, 0x5fd7af, 0x5fd7d7, 0x5fd7ff,
  0x5fff00, 0x5fff5f, 0x5fff87, 0x5fffaf, 0x5fffd7, 0x5fffff,
  // 88
  0x870000, 0x87005f, 0x870087, 0x8700af, 0x8700d7, 0x8700ff,
  0x875f00, 0x875f5f, 0x875f87, 0x875faf, 0x875fd7, 0x875fff,
  0x878700, 0x87875f, 0x878787, 0x8787af, 0x8787d7, 0x8787ff,
  0x87af00, 0x87af5f, 0x87af87, 0x87afaf, 0x87afd7, 0x87afff,
  0x87d700, 0x87d75f, 0x87d787, 0x87d7af, 0x87d7d7, 0x87d7ff,
  0x87ff00, 0x87ff5f, 0x87ff87, 0x87ffaf, 0x87ffd7, 0x87ffff,
  // 124
  0xaf0000, 0xaf005f, 0xaf0087, 0xaf00af, 0xaf00d7, 0xaf00ff,
  0xaf5f00, 0xaf5f5f, 0xaf5f87, 0xaf5faf, 0xaf5fd7, 0xaf5fff,
  0xaf8700, 0xaf875f, 0xaf8787, 0xaf87af, 0xaf87d7, 0xaf87ff,
  0xafaf00, 0xafaf5f, 0xafaf87, 0xafafaf, 0xafafd7, 0xafafff,
  0xafd700, 0xafd75f, 0xafd787, 0xafd7af, 0xafd7d7, 0xafd7ff,
  0xafff00, 0xafff5f, 0xafff87, 0xafffaf, 0xafffd7, 0xafffff,
  // 160
  0xd70000, 0xd7005f, 0xd70087, 0xd700af, 0xd700d7, 0xd700ff,
  0xd75f00, 0xd75f5f, 0xd75f87, 0xd75faf, 0xd75fd7, 0xd75fff,
  0xd78700, 0xd7875f, 0xd78787, 0xd787af, 0xd787d7, 0xd787ff,
  0xd7af00, 0xd7af5f, 0xd7af87, 0xd7afaf, 0xd7afd7, 0xd7afff,
  0xd7d700, 0xd7d75f, 0xd7d787, 0xd7d7af, 0xd7d7d7, 0xd7d7ff,
  0xd7ff00, 0xd7ff5f, 0xd7ff87, 0xd7ffaf, 0xd7ffd7, 0xd7ffff,
  // 196
  0xff0000, 0xff005f, 0xff0087, 0xff00af, 0xff00d7, 0xff00ff,
  0xff5f00, 0xff5f5f, 0xff5f87, 0xff5faf, 0xff5fd7, 0xff5fff,
  0xff8700, 0xff875f, 0xff8787, 0xff87af, 0xff87d7, 0xff87ff,
  0xffaf00, 0xffaf5f, 0xffaf87, 0xffafaf, 0xffafd7, 0xffafff,
  0xffd700, 0xffd75f, 0xffd787, 0xffd7af, 0xffd7d7, 0xffd7ff,
  0xffff00, 0xffff5f, 0xffff87, 0xffffaf, 0xffffd7, 0xffffff,
  // 232, gray scale
  0x080808, 0x121212, 0x1c1c1c, 0x262626, 0x303030, 0x3a3a3a, 
  0x444444, 0x4e4e4e, 0x585858, 0x626262, 0x6c6c6c, 0x767676, 
  0x808080, 0x8a8a8a, 0x949494, 0x9e9e9e, 0xa8a8a8, 0xb2b2b2, 
  0xbcbcbc, 0xc6c6c6, 0xd0d0d0, 0xdadada, 0xe4e4e4, 0xeeeeee   
};


//-------------------------------------------------------------
// Create colors
//-------------------------------------------------------------

// Create a color from a 24-bit color value.
ic_private ic_color_t ic_rgb(uint32_t hex) {
  return (ic_color_t)(0x1000000 | (hex & 0xFFFFFF));
}

// Limit an int to values between 0 and 255.
static uint32_t ic_cap8(ssize_t i) {
  return (i < 0 ? 0 : (i > 255 ? 255 : (uint32_t)i));
}

// Create a color from a 24-bit color value.
ic_private ic_color_t ic_rgbx(ssize_t r, ssize_t g, ssize_t b) {
  return ic_rgb( (ic_cap8(r)<<16) | (ic_cap8(g)<<8) | ic_cap8(b) );
}


//-------------------------------------------------------------
// Match an rgb color to a ansi8, ansi16, or ansi256
//-------------------------------------------------------------

static bool color_is_rgb( ic_color_t color ) {
  return (color >= IC_RGB(0));  // bit 24 is set for rgb colors
}

static void color_to_rgb(ic_color_t color, int* r, int* g, int* b) {
  assert(color_is_rgb(color));
  *r = ((color >> 16) & 0xFF);
  *g = ((color >> 8) & 0xFF);
  *b = (color & 0xFF);
}

ic_private ic_color_t color_from_ansi256(ssize_t i) {
  if (i >= 0 && i < 8) {
    return (IC_ANSI_BLACK + (uint32_t)i);
  }
  else if (i >= 8 && i < 16) {
    return (IC_ANSI_DARKGRAY + (uint32_t)(i - 8));
  }
  else if (i >= 16 && i <= 255) {
    return ic_rgb( ansi256[i] );
  }
  else if (i == 256) {
    return IC_ANSI_DEFAULT;
  }
  else {
    return IC_ANSI_DEFAULT;
  }
}

static bool is_grayish(int r, int g, int b) {
  return (abs(r-g) <= 4) && (abs((r+g)/2 - b) <= 4);
}

static bool is_grayish_color( uint32_t rgb ) {
  int r, g, b;
  color_to_rgb(IC_RGB(rgb),&r,&g,&b);
  return is_grayish(r,g,b);
}

static int_least32_t sqr(int_least32_t x) {
  return x*x;
}

// Approximation to delta-E CIE color distance using much 
// simpler calculations. See <https://www.compuphase.com/cmetric.htm>.
// This is essentialy weighted euclidean distance but the weight distribution
// depends on how big the "red" component of the color is.
// We do not take the square root as we only need to find 
// the minimal distance (and multiply by 256 to increase precision).
// Needs at least 28-bit signed integers to avoid overflow. 
static int_least32_t rgb_distance_rmean( uint32_t color, int r2, int g2, int b2 ) {
  int r1, g1, b1;
  color_to_rgb(IC_RGB(color),&r1,&g1,&b1);
  int_least32_t rmean = (r1 + r2) / 2;
  int_least32_t dr2 = sqr(r1 - r2);
  int_least32_t dg2 = sqr(g1 - g2);
  int_least32_t db2 = sqr(b1 - b2);
  int_least32_t dist = ((512+rmean)*dr2) + 1024*dg2 + ((767-rmean)*db2);    
  return dist;
}

// Another approximation to delta-E CIE color distance using
// simpler calculations. Similar to `rmean` but adds an adjustment factor
// based on the "red/blue" difference.
static int_least32_t rgb_distance_rbmean( uint32_t color, int r2, int g2, int b2 ) {
  int r1, g1, b1;
  color_to_rgb(IC_RGB(color),&r1,&g1,&b1);
  int_least32_t rmean = (r1 + r2) / 2;
  int_least32_t dr2 = sqr(r1 - r2);
  int_least32_t dg2 = sqr(g1 - g2);
  int_least32_t db2 = sqr(b1 - b2);
  int_least32_t dist = 2*dr2 + 4*dg2 + 3*db2 + ((rmean*(dr2 - db2))/256);  
  return dist;
}


// Maintain a small cache of recently used colors. Should be short enough to be effectively constant time.
// If we ever use a more expensive color distance method, we may increase the size a bit (64?) 
// (Initial zero initialized cache is valid.)
#define RGB_CACHE_LEN (16)
typedef struct rgb_cache_s {
  int        last;
  int        indices[RGB_CACHE_LEN];
  ic_color_t colors[RGB_CACHE_LEN];
} rgb_cache_t;

// remember a color in the LRU cache
void rgb_remember( rgb_cache_t* cache, ic_color_t color, int idx ) {
  if (cache == NULL) return;
  cache->colors[cache->last] = color;
  cache->indices[cache->last] = idx;
  cache->last++;
  if (cache->last >= RGB_CACHE_LEN) { cache->last = 0; }
}

// quick lookup in cache; -1 on failure
int rgb_lookup( const rgb_cache_t* cache, ic_color_t color ) {
  if (cache != NULL) {
    for(int i = 0; i < RGB_CACHE_LEN; i++) {
      if (cache->colors[i] == color) return cache->indices[i];
    }
  }
  return -1;
}

// return the index of the closest matching color
static int rgb_match( uint32_t* palette, int start, int len, rgb_cache_t* cache, ic_color_t color ) {
  assert(color_is_rgb(color));
  // in cache?
  int min = rgb_lookup(cache,color);
  if (min >= 0) {
    return min;
  }
  // otherwise find closest color match in the palette
  int r, g, b;
  color_to_rgb(color,&r,&g,&b);
  min = start;
  int_least32_t mindist = (INT_LEAST32_MAX)/4;
  for(int i = start; i < len; i++) {
    //int_least32_t dist = rgb_distance_rbmean(palette[i],r,g,b);
    int_least32_t dist = rgb_distance_rmean(palette[i],r,g,b);
    if (is_grayish_color(palette[i]) != is_grayish(r, g, b)) { 
      // with few colors, make it less eager to substitute a gray for a non-gray (or the other way around)
      if (len <= 16) {
        dist *= 4;
      } 
      else {
        dist = (dist/4)*5;
      }
    } 
    if (dist < mindist) {
      min = i;
      mindist = dist;
    }
  }
  rgb_remember(cache,color,min);
  return min;
}


// Match RGB to an index in the ANSI 256 color table
static int rgb_to_ansi256(ic_color_t color) {
  static rgb_cache_t ansi256_cache;
  int c = rgb_match(ansi256, 16, 256, &ansi256_cache, color); // not the first 16 ANSI colors as those may be different 
  //debug_msg("term: rgb %x -> ansi 256: %d\n", color, c );
  return c;
}

// Match RGB to an ANSI 16 color code (30-37, 90-97)
static int color_to_ansi16(ic_color_t color) {
  if (!color_is_rgb(color)) {
    return (int)color;
  }
  else {
    static rgb_cache_t ansi16_cache;
    int c = rgb_match(ansi256, 0, 16, &ansi16_cache, color);
    //debug_msg("term: rgb %x -> ansi 16: %d\n", color, c );
    return (c < 8 ? 30 + c : 90 + c - 8); 
  }
}

// Match RGB to an ANSI 16 color code (30-37, 90-97)
// but assuming the bright colors are simulated using 'bold'.
static int color_to_ansi8(ic_color_t color) {
  if (!color_is_rgb(color)) {
    return (int)color;
  }
  else {
    // match to basic 8 colors first
    static rgb_cache_t ansi8_cache;
    int c = 30 + rgb_match(ansi256, 0, 8, &ansi8_cache, color);
    // and then adjust for brightness
    int r, g, b;
    color_to_rgb(color,&r,&g,&b);
    if (r>=196 || g>=196 || b>=196) c += 60;
    //debug_msg("term: rgb %x -> ansi 8: %d\n", color, c );
    return c;
  }
}


//-------------------------------------------------------------
// Emit color escape codes based on the terminal capability
//-------------------------------------------------------------

static void fmt_color_ansi8( char* buf, ssize_t len, ic_color_t color, bool bg ) {
  int c = color_to_ansi8(color) + (bg ? 10 : 0);
  if (c >= 90) {
    snprintf(buf, to_size_t(len), IC_CSI "1;%dm", c - 60);    
  }
  else {
    snprintf(buf, to_size_t(len), IC_CSI "22;%dm", c );  
  }
}

static void fmt_color_ansi16( char* buf, ssize_t len, ic_color_t color, bool bg ) {
  snprintf( buf, to_size_t(len), IC_CSI "%dm", color_to_ansi16(color) + (bg ? 10 : 0) );  
}

static void fmt_color_ansi256( char* buf, ssize_t len,  ic_color_t color, bool bg ) {
  if (!color_is_rgb(color)) {
    fmt_color_ansi16(buf,len,color,bg);
  }
  else {
    snprintf( buf, to_size_t(len), IC_CSI "%d;5;%dm", (bg ? 48 : 38), rgb_to_ansi256(color) );  
  }
}

static void fmt_color_rgb( char* buf, ssize_t len, ic_color_t color, bool bg ) {
  if (!color_is_rgb(color)) {
    fmt_color_ansi16(buf,len,color,bg);
  }
  else {
    int r,g,b;
    color_to_rgb(color, &r,&g,&b);
    snprintf( buf, to_size_t(len), IC_CSI "%d;2;%d;%d;%dm", (bg ? 48 : 38), r, g, b );  
  }
}

static void fmt_color_ex(char* buf, ssize_t len, palette_t palette, ic_color_t color, bool bg) {
  if (color == IC_COLOR_NONE || palette == MONOCHROME) return;
  if (palette == ANSI8) {
    fmt_color_ansi8(buf,len,color,bg);
  }
  else if (!color_is_rgb(color) || palette == ANSI16) {
    fmt_color_ansi16(buf,len,color,bg);
  }
  else if (palette == ANSI256) {
    fmt_color_ansi256(buf,len,color,bg);
  }
  else {
    fmt_color_rgb(buf,len,color,bg);
  }
}

static void term_color_ex(term_t* term, ic_color_t color, bool bg) {
  char buf[128+1];
  fmt_color_ex(buf,128,term->palette,color,bg);
  term_write(term,buf);
}

//-------------------------------------------------------------
// Main API functions
//-------------------------------------------------------------

ic_private void term_color(term_t* term, ic_color_t color) {
  term_color_ex(term,color,false);
}

ic_private void term_bgcolor(term_t* term, ic_color_t color) {
  term_color_ex(term,color,true);
}

ic_private void term_append_color(term_t* term, stringbuf_t* sbuf, ic_color_t color) {
  char buf[128+1];
  fmt_color_ex(buf,128,term->palette,color,false);
  sbuf_append(sbuf,buf);
}

ic_private void term_append_bgcolor(term_t* term, stringbuf_t* sbuf, ic_color_t color) {
  char buf[128+1];
  fmt_color_ex(buf, 128, term->palette, color, true);
  sbuf_append(sbuf, buf);
}

ic_private int term_get_color_bits(term_t* term) {
  switch (term->palette) {
  case MONOCHROME: return 1;
  case ANSI8:      return 3;
  case ANSI16:     return 4;
  case ANSI256:    return 8;
  case ANSIRGB:    return 24;
  default:         return 4;
  }
}
