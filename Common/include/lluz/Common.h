// This file is part of the lluz programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

// Compiler codegen control macros
#ifdef _MSC_VER
#define lluz_NORETURN __declspec(noreturn)
#define lluz_NOINLINE __declspec(noinline)
#define lluz_FORCEINLINE __forceinline
#define lluz_LIKELY(x) x
#define lluz_UNLIKELY(x) x
#define lluz_UNREACHABLE() __assume(false)
#define lluz_DEBUGBREAK() __debugbreak()
#else
#define lluz_NORETURN __attribute__((__noreturn__))
#define lluz_NOINLINE __attribute__((noinline))
#define lluz_FORCEINLINE inline __attribute__((always_inline))
#define lluz_LIKELY(x) __builtin_expect(x, 1)
#define lluz_UNLIKELY(x) __builtin_expect(x, 0)
#define lluz_UNREACHABLE() __builtin_unreachable()
#define lluz_DEBUGBREAK() __builtin_trap()
#endif







namespace lluz
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

} // namespace lluz

#if !defined(NDEBUG) || defined(lluz_ENABLE_ASSERT)
#define lluz_ASSERT(expr) ((void)(!!(expr) || (lluz::assertCallHandler(#expr, __FILE__, __LINE__, __FUNCTION__) && (lluz_DEBUGBREAK(), 0))))
#define lluz_ASSERTENABLED
#else
#define lluz_ASSERT(expr) (void)sizeof(!!(expr))
#endif

namespace lluz
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

} // namespace lluz

#define lluz_FASTFLAG(flag) \
    namespace FFlag \
    { \
    extern lluz::FValue<bool> flag; \
    }
#define lluz_FASTFLAGVARIABLE(flag, def) \
    namespace FFlag \
    { \
    lluz::FValue<bool> flag(#flag, def, false, nullptr); \
    }
#define lluz_FASTINT(flag) \
    namespace FInt \
    { \
    extern lluz::FValue<int> flag; \
    }
#define lluz_FASTINTVARIABLE(flag, def) \
    namespace FInt \
    { \
    lluz::FValue<int> flag(#flag, def, false, nullptr); \
    }

#define lluz_DYNAMIC_FASTFLAG(flag) \
    namespace DFFlag \
    { \
    extern lluz::FValue<bool> flag; \
    }
#define lluz_DYNAMIC_FASTFLAGVARIABLE(flag, def) \
    namespace DFFlag \
    { \
    lluz::FValue<bool> flag(#flag, def, true, nullptr); \
    }
#define lluz_DYNAMIC_FASTINT(flag) \
    namespace DFInt \
    { \
    extern lluz::FValue<int> flag; \
    }
#define lluz_DYNAMIC_FASTINTVARIABLE(flag, def) \
    namespace DFInt \
    { \
    lluz::FValue<int> flag(#flag, def, true, nullptr); \
    }
