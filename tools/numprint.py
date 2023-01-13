#!/usr/bin/python3
# This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

# This code can be used to generate power tables for Schubfach algorithm (see lnumprint.cpp)

import math
import sys

(_, pow10min, pow10max, compact) = sys.argv
pow10min = int(pow10min)
pow10max = int(pow10max)
compact = compact == "True"

# extract high 128 bits of the value
def high128(n, roundup):
	L = math.ceil(math.log2(n))

	r = 0
	for i in range(L - 128, L):
		if i >= 0 and (n & (1 << i)) != 0:
			r |= (1 << (i - L + 128))

	return r + (1 if roundup else 0)

def pow10approx(n):
	if n == 0:
		return 1 << 127
	elif n > 0:
		return high128(10**n, 5**n >= 2**128)
	else:
		# 10^-n is a binary fraction that can't be represented in floating point
		# we need to extract top 128 bits of the fraction starting from the first 1
		# to get there, we need to divide 2^k by 10^n for a sufficiently large k and repeat the extraction process
		p = 10**-n
		k = 2**128 * 16**-n # this guarantees that the fraction has more than 128 extra bits
		return high128(k // p, True)

def pow5_64(n):
	assert(n >= 0)
	if n == 0:
		return 1 << 63
	else:
		return high128(5**n, False) >> 64

if not compact:
	print("// kPow10Table", pow10min, "..", pow10max)
	print("{")
	for p in range(pow10min, pow10max + 1):
		h = hex(pow10approx(p))[2:]
		assert(len(h) == 32)
		print("    {0x%s, 0x%s}," % (h[0:16].upper(), h[16:32].upper()))
	print("}")
else:
	print("// kPow5Table")
	print("{")
	for i in range(16):
		print("    " + hex(pow5_64(i)) + ",")
	print("}")
	print("// kPow10Table", pow10min, "..", pow10max)
	print("{")
	for p in range(pow10min, pow10max + 1, 16):
		base = pow10approx(p)
		errw = 0
		for i in range(16):
			real = pow10approx(p + i)
			appr = (base * pow5_64(i)) >> 64
			scale = 1 if appr < (1 << 127) else 0 # 1-bit scale

			offset = (appr << scale) - real
			assert(offset >= -4 and offset <= 3) # 3-bit offset
			assert((appr << scale) >> 64 == real >> 64) # offset only affects low half
			assert((appr << scale) - offset == real) # validate full reconstruction

			err = (scale << 3) | (offset + 4)
			errw |= err << (i * 4)

		hbase = hex(base)[2:]
		assert(len(hbase) == 32)
		assert(errw < 1 << 64)

		print("    {0x%s, 0x%s, 0x%16x}," % (hbase[0:16], hbase[16:32], errw))
	print("}")
