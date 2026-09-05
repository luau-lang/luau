print("=== BASIC ===")

print("5 & 3  =", 5 & 3)
print("5 | 3  =", 5 | 3)
print("5 ~ 3  =", 5 ~ 3)
print("5 << 2 =", 5 << 2)
print("20 >> 2 =", 20 >> 2)
print("~5     =", ~5)

print("=== VARIABLES ===")

local a = 5
local b = 3

print(a & b)
print(a | b)
print(a ~ b)

print("=== PRECEDENCE ===")

print(1 | 2 & 4)
print(1 + 2 & 7)
print(8 >> 1 + 1)

print("=== DONE ===")