#pragma once

#include "Luau/Common.h"

#include <stdint.h>

namespace Luau
{
namespace CodeGen
{

enum class SizeX64 : uint8_t
{
    none,
    byte,
    word,
    dword,
    qword,
    xmmword,
    ymmword,
};

struct RegisterX64
{
    SizeX64 size : 3;
    uint8_t index : 5;

    constexpr bool operator==(RegisterX64 rhs) const
    {
        return size == rhs.size && index == rhs.index;
    }

    constexpr bool operator!=(RegisterX64 rhs) const
    {
        return !(*this == rhs);
    }
};

constexpr RegisterX64 noreg{SizeX64::none, 16};
constexpr RegisterX64 rip{SizeX64::none, 0};

constexpr RegisterX64 al{SizeX64::byte, 0};
constexpr RegisterX64 cl{SizeX64::byte, 1};
constexpr RegisterX64 dl{SizeX64::byte, 2};
constexpr RegisterX64 bl{SizeX64::byte, 3};

constexpr RegisterX64 eax{SizeX64::dword, 0};
constexpr RegisterX64 ecx{SizeX64::dword, 1};
constexpr RegisterX64 edx{SizeX64::dword, 2};
constexpr RegisterX64 ebx{SizeX64::dword, 3};
constexpr RegisterX64 esp{SizeX64::dword, 4};
constexpr RegisterX64 ebp{SizeX64::dword, 5};
constexpr RegisterX64 esi{SizeX64::dword, 6};
constexpr RegisterX64 edi{SizeX64::dword, 7};
constexpr RegisterX64 r8d{SizeX64::dword, 8};
constexpr RegisterX64 r9d{SizeX64::dword, 9};
constexpr RegisterX64 r10d{SizeX64::dword, 10};
constexpr RegisterX64 r11d{SizeX64::dword, 11};
constexpr RegisterX64 r12d{SizeX64::dword, 12};
constexpr RegisterX64 r13d{SizeX64::dword, 13};
constexpr RegisterX64 r14d{SizeX64::dword, 14};
constexpr RegisterX64 r15d{SizeX64::dword, 15};

constexpr RegisterX64 rax{SizeX64::qword, 0};
constexpr RegisterX64 rcx{SizeX64::qword, 1};
constexpr RegisterX64 rdx{SizeX64::qword, 2};
constexpr RegisterX64 rbx{SizeX64::qword, 3};
constexpr RegisterX64 rsp{SizeX64::qword, 4};
constexpr RegisterX64 rbp{SizeX64::qword, 5};
constexpr RegisterX64 rsi{SizeX64::qword, 6};
constexpr RegisterX64 rdi{SizeX64::qword, 7};
constexpr RegisterX64 r8{SizeX64::qword, 8};
constexpr RegisterX64 r9{SizeX64::qword, 9};
constexpr RegisterX64 r10{SizeX64::qword, 10};
constexpr RegisterX64 r11{SizeX64::qword, 11};
constexpr RegisterX64 r12{SizeX64::qword, 12};
constexpr RegisterX64 r13{SizeX64::qword, 13};
constexpr RegisterX64 r14{SizeX64::qword, 14};
constexpr RegisterX64 r15{SizeX64::qword, 15};

constexpr RegisterX64 xmm0{SizeX64::xmmword, 0};
constexpr RegisterX64 xmm1{SizeX64::xmmword, 1};
constexpr RegisterX64 xmm2{SizeX64::xmmword, 2};
constexpr RegisterX64 xmm3{SizeX64::xmmword, 3};
constexpr RegisterX64 xmm4{SizeX64::xmmword, 4};
constexpr RegisterX64 xmm5{SizeX64::xmmword, 5};
constexpr RegisterX64 xmm6{SizeX64::xmmword, 6};
constexpr RegisterX64 xmm7{SizeX64::xmmword, 7};
constexpr RegisterX64 xmm8{SizeX64::xmmword, 8};
constexpr RegisterX64 xmm9{SizeX64::xmmword, 9};
constexpr RegisterX64 xmm10{SizeX64::xmmword, 10};
constexpr RegisterX64 xmm11{SizeX64::xmmword, 11};
constexpr RegisterX64 xmm12{SizeX64::xmmword, 12};
constexpr RegisterX64 xmm13{SizeX64::xmmword, 13};
constexpr RegisterX64 xmm14{SizeX64::xmmword, 14};
constexpr RegisterX64 xmm15{SizeX64::xmmword, 15};

constexpr RegisterX64 ymm0{SizeX64::ymmword, 0};
constexpr RegisterX64 ymm1{SizeX64::ymmword, 1};
constexpr RegisterX64 ymm2{SizeX64::ymmword, 2};
constexpr RegisterX64 ymm3{SizeX64::ymmword, 3};
constexpr RegisterX64 ymm4{SizeX64::ymmword, 4};
constexpr RegisterX64 ymm5{SizeX64::ymmword, 5};
constexpr RegisterX64 ymm6{SizeX64::ymmword, 6};
constexpr RegisterX64 ymm7{SizeX64::ymmword, 7};
constexpr RegisterX64 ymm8{SizeX64::ymmword, 8};
constexpr RegisterX64 ymm9{SizeX64::ymmword, 9};
constexpr RegisterX64 ymm10{SizeX64::ymmword, 10};
constexpr RegisterX64 ymm11{SizeX64::ymmword, 11};
constexpr RegisterX64 ymm12{SizeX64::ymmword, 12};
constexpr RegisterX64 ymm13{SizeX64::ymmword, 13};
constexpr RegisterX64 ymm14{SizeX64::ymmword, 14};
constexpr RegisterX64 ymm15{SizeX64::ymmword, 15};

} // namespace CodeGen
} // namespace Luau
