// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

// Compiler codegen control macros
#ifdef _MSC_VER
#define LUAU_NORETURN __declspec(noreturn)
#define LUAU_NOINLINE __declspec(noinline)
#define LUAU_FORCEINLINE __forceinline
#define LUAU_LIKELY(x) x
#define LUAU_UNLIKELY(x) x
#define LUAU_UNREACHABLE() __assume(false)
#define LUAU_DEBUGBREAK() __debugbreak()
#else
#define LUAU_NORETURN __attribute__((__noreturn__))
#define LUAU_NOINLINE __attribute__((noinline))
#define LUAU_FORCEINLINE inline __attribute__((always_inline))
#define LUAU_LIKELY(x) __builtin_expect(x, 1)
#define LUAU_UNLIKELY(x) __builtin_expect(x, 0)
#define LUAU_UNREACHABLE() __builtin_unreachable()
#define LUAU_DEBUGBREAK() __builtin_trap()
#endif







namespace Luau
{

using AssertHandler = int (*)(const char* expression, const char* file, int line, const char* function);

inline AssertHandler& assertHandler()
{
    static AssertHandler handler = nullptr;
    return handler;
}

inline int assertCallHandler(const char* expression, const char* file, int line, const char* function)
{
    if (AssertHandler handler = assertHandler())
        return handler(expression, file, line, function);

    return 1;
}

} // namespace Luau

#if !defined(NDEBUG) || defined(LUAU_ENABLE_ASSERT)
#define LUAU_ASSERT(expr) ((void)(!!(expr) || (Luau::assertCallHandler(#expr, __FILE__, __LINE__, __FUNCTION__) && (LUAU_DEBUGBREAK(), 0))))
#define LUAU_ASSERTENABLED
#else
#define LUAU_ASSERT(expr) (void)sizeof(!!(expr))
#endif

namespace Luau
{

template<typename T>
struct FValue
{
    static FValue* list;

    T value;
    bool dynamic;
    const char* name;
    FValue* next;

    FValue(const char* name, T def, bool dynamic, void (*reg)(const char*, T*, bool) = nullptr)
        : value(def)
        , dynamic(dynamic)
        , name(name)
        , next(list)
    {
        list = this;

        if (reg)
            reg(name, &value, dynamic);
    }

    operator T() const
    {
        return value;
    }
};

template<typename T>
FValue<T>* FValue<T>::list = nullptr;

} // namespace Luau

#define LUAU_FASTFLAG(flag) \
    namespace FFlag \
    { \
    extern Luau::FValue<bool> flag; \
    }
#define LUAU_FASTFLAGVARIABLE(flag, def) \
    namespace FFlag \
    { \
    Luau::FValue<bool> flag(#flag, def, false, nullptr); \
    }
#define LUAU_FASTINT(flag) \
    namespace FInt \
    { \
    extern Luau::FValue<int> flag; \
    }
#define LUAU_FASTINTVARIABLE(flag, def) \
    namespace FInt \
    { \
    Luau::FValue<int> flag(#flag, def, false, nullptr); \
    }

#define LUAU_DYNAMIC_FASTFLAG(flag) \
    namespace DFFlag \
    { \
    extern Luau::FValue<bool> flag; \
    }
#define LUAU_DYNAMIC_FASTFLAGVARIABLE(flag, def) \
    namespace DFFlag \
    { \
    Luau::FValue<bool> flag(#flag, def, true, nullptr); \
    }
#define LUAU_DYNAMIC_FASTINT(flag) \
    namespace DFInt \
    { \
    extern Luau::FValue<int> flag; \
    }
#define LUAU_DYNAMIC_FASTINTVARIABLE(flag, def) \
    namespace DFInt \
    { \
    Luau::FValue<int> flag(#flag, def, true, nullptr); \
    }
