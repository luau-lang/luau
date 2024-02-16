// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/Common.h"

#if defined(LUAU_ASSERTENABLED)
#define CODEGEN_ASSERT(expr) ((void)(!!(expr) || (Luau::assertCallHandler(#expr, __FILE__, __LINE__, __FUNCTION__) && (LUAU_DEBUGBREAK(), 0))))
#elif defined(CODEGEN_ENABLE_ASSERT_HANDLER)
#define CODEGEN_ASSERT(expr) ((void)(!!(expr) || Luau::assertCallHandler(#expr, __FILE__, __LINE__, __FUNCTION__)))
#else
#define CODEGEN_ASSERT(expr) (void)sizeof(!!(expr))
#endif
