// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include <stdint.h>

// Buffers passed to these functions must have room for this many characters; the output isn't null-terminated
#define LUAI_MAXNUM2STR 48
#define LUAI_MAXINT2STR 30

// Writes the shortest decimal representation of a number that reads back as exactly the same value, returning the end of
// the output. Infinities and NaN are written as `inf`, `-inf` and `nan`.
char* luai_num2str(char* buf, double n);

// Like luai_num2str, but for the shortest representation that reads back as exactly the same float when converted
// directly to a float (as by strtof). Converting to a double first and then to a float can round differently.
char* luai_num2strf(char* buf, float n);

char* luai_int2str(char* buf, int64_t n);
