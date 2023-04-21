#!/usr/bin/python3
# This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

# Given the output of --compile=codegenverbose in stdin, this script outputs statistics about bytecode/IR

import sys
import re
from collections import defaultdict

count_bc = defaultdict(int)
count_ir = defaultdict(int)
count_asm = defaultdict(int)
count_irasm = defaultdict(int)

# GETTABLEKS R10 R1 K18 ['s']
# L1: DIV R14 R13 R3
re_bc = re.compile(r'^(?:L\d+: )?([A-Z_]+) ')
# #   CHECK_SLOT_MATCH %178, K3, bb_fallback_37
# #   %175 = LOAD_TAG R15
re_ir = re.compile(r'^#   (?:%\d+ = )?([A-Z_]+) ')
#  cmp         w14,#5
re_asm = re.compile(r'^ ([a-z.]+) ')

current_ir = None

for line in sys.stdin.buffer.readlines():
    line = line.decode('utf-8', errors='ignore').rstrip()

    if m := re_asm.match(line):
        count_asm[m[1]] += 1
        if current_ir:
            count_irasm[current_ir] += 1
    elif m := re_ir.match(line):
        count_ir[m[1]] += 1
        current_ir = m[1]
    elif m := re_bc.match(line):
        count_bc[m[1]] += 1

def display(name, counts, limit=None, extra=None):
    items = sorted(counts.items(), key=lambda p: p[1], reverse=True)
    total = 0
    for k,v in items:
        total += v
    shown = 0
    print(name)
    for i, (k,v) in enumerate(items):
        if i == limit:
            if shown < total:
                print(f'  {"Others":25}: {total-shown} ({(total-shown)/total*100:.1f}%)')
            break
        print(f'  {k:25}: {v} ({v/total*100:.1f}%){"; "+extra(k) if extra else ""}')
        shown += v
    print()

display("Bytecode", count_bc, limit=20)
display("IR", count_ir, limit=20)
display("Assembly", count_asm, limit=10)
display("IR->Assembly", count_irasm, limit=30, extra=lambda op: f'{count_irasm[op] / count_ir[op]:.1f} insn/op')
