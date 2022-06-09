#pragma once

#include "Luau/Common.h"
#include "Luau/RegisterX64.h"

#include <stdint.h>

namespace Luau
{
namespace CodeGen
{

enum class CategoryX64 : uint8_t
{
    reg,
    mem,
    imm,
};

struct OperandX64
{
    constexpr OperandX64(RegisterX64 reg)
        : cat(CategoryX64::reg)
        , index(noreg)
        , base(reg)
        , memSize(SizeX64::none)
        , scale(1)
        , imm(0)
    {
    }

    constexpr OperandX64(int32_t imm)
        : cat(CategoryX64::imm)
        , index(noreg)
        , base(noreg)
        , memSize(SizeX64::none)
        , scale(1)
        , imm(imm)
    {
    }

    constexpr explicit OperandX64(SizeX64 size, RegisterX64 index, uint8_t scale, RegisterX64 base, int32_t disp)
        : cat(CategoryX64::mem)
        , index(index)
        , base(base)
        , memSize(size)
        , scale(scale)
        , imm(disp)
    {
    }

    // Fields are carefully placed to make this struct fit into an 8 byte register
    CategoryX64 cat;
    RegisterX64 index;
    RegisterX64 base;
    SizeX64 memSize : 4;
    uint8_t scale : 4;
    int32_t imm;

    constexpr OperandX64 operator[](OperandX64&& addr) const
    {
        LUAU_ASSERT(cat == CategoryX64::mem);
        LUAU_ASSERT(memSize != SizeX64::none && index == noreg && scale == 1 && base == noreg && imm == 0);
        LUAU_ASSERT(addr.memSize == SizeX64::none);

        addr.cat = CategoryX64::mem;
        addr.memSize = memSize;
        return addr;
    }
};

constexpr OperandX64 byte{SizeX64::byte, noreg, 1, noreg, 0};
constexpr OperandX64 word{SizeX64::word, noreg, 1, noreg, 0};
constexpr OperandX64 dword{SizeX64::dword, noreg, 1, noreg, 0};
constexpr OperandX64 qword{SizeX64::qword, noreg, 1, noreg, 0};
constexpr OperandX64 xmmword{SizeX64::xmmword, noreg, 1, noreg, 0};
constexpr OperandX64 ymmword{SizeX64::ymmword, noreg, 1, noreg, 0};
constexpr OperandX64 ptr{sizeof(void*) == 4 ? SizeX64::dword : SizeX64::qword, noreg, 1, noreg, 0};

constexpr OperandX64 operator*(RegisterX64 reg, uint8_t scale)
{
    if (scale == 1)
        return OperandX64(reg);

    LUAU_ASSERT(scale == 1 || scale == 2 || scale == 4 || scale == 8);
    LUAU_ASSERT(reg.index != 0b100 && "can't scale SP");

    return OperandX64(SizeX64::none, reg, scale, noreg, 0);
}

constexpr OperandX64 operator+(RegisterX64 reg, int32_t disp)
{
    return OperandX64(SizeX64::none, noreg, 1, reg, disp);
}

constexpr OperandX64 operator+(RegisterX64 base, RegisterX64 index)
{
    LUAU_ASSERT(index.index != 4 && "sp cannot be used as index");
    LUAU_ASSERT(base.size == index.size);

    return OperandX64(SizeX64::none, index, 1, base, 0);
}

constexpr OperandX64 operator+(OperandX64 op, int32_t disp)
{
    LUAU_ASSERT(op.cat == CategoryX64::mem);
    LUAU_ASSERT(op.memSize == SizeX64::none);

    op.imm += disp;
    return op;
}

constexpr OperandX64 operator+(OperandX64 op, RegisterX64 base)
{
    LUAU_ASSERT(op.cat == CategoryX64::mem);
    LUAU_ASSERT(op.memSize == SizeX64::none);
    LUAU_ASSERT(op.base == noreg);
    LUAU_ASSERT(op.index == noreg || op.index.size == base.size);

    op.base = base;
    return op;
}

constexpr OperandX64 operator+(RegisterX64 base, OperandX64 op)
{
    LUAU_ASSERT(op.cat == CategoryX64::mem);
    LUAU_ASSERT(op.memSize == SizeX64::none);
    LUAU_ASSERT(op.base == noreg);
    LUAU_ASSERT(op.index == noreg || op.index.size == base.size);

    op.base = base;
    return op;
}

} // namespace CodeGen
} // namespace Luau
