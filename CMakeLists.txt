# This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
if(EXT_PLATFORM_STRING)
    include(EXTLuau.cmake)
    return()
endif()

cmake_minimum_required(VERSION 3.0)

option(LUAU_BUILD_CLI "Build CLI" ON)
option(LUAU_BUILD_TESTS "Build tests" ON)
option(LUAU_BUILD_WEB "Build Web module" OFF)
option(LUAU_WERROR "Warnings as errors" OFF)
option(LUAU_STATIC_CRT "Link with the static CRT (/MT)" OFF)

if(LUAU_STATIC_CRT)
    cmake_minimum_required(VERSION 3.15)
    cmake_policy(SET CMP0091 NEW)
    set(CMAKE_MSVC_RUNTIME_LIBRARY "MultiThreaded$<$<CONFIG:Debug>:Debug>")
endif()

project(Luau LANGUAGES CXX C)
add_library(Luau.Ast STATIC)
add_library(Luau.Compiler STATIC)
add_library(Luau.Analysis STATIC)
add_library(Luau.VM STATIC)
add_library(isocline STATIC)

if(LUAU_BUILD_CLI)
    add_executable(Luau.Repl.CLI)
    add_executable(Luau.Analyze.CLI)
    add_executable(Luau.Ast.CLI)

    # This also adds target `name` on Linux/macOS and `name.exe` on Windows
    set_target_properties(Luau.Repl.CLI PROPERTIES OUTPUT_NAME luau)
    set_target_properties(Luau.Analyze.CLI PROPERTIES OUTPUT_NAME luau-analyze)
    set_target_properties(Luau.Ast.CLI PROPERTIES OUTPUT_NAME luau-ast)
endif()

if(LUAU_BUILD_TESTS)
    add_executable(Luau.UnitTest)
    add_executable(Luau.Conformance)
    add_executable(Luau.CLI.Test)
endif()

if(LUAU_BUILD_WEB)
    add_executable(Luau.Web)
endif()

include(Sources.cmake)

target_compile_features(Luau.Ast PUBLIC cxx_std_17)
target_include_directories(Luau.Ast PUBLIC Ast/include)

target_compile_features(Luau.Compiler PUBLIC cxx_std_17)
target_include_directories(Luau.Compiler PUBLIC Compiler/include)
target_link_libraries(Luau.Compiler PUBLIC Luau.Ast)

target_compile_features(Luau.Analysis PUBLIC cxx_std_17)
target_include_directories(Luau.Analysis PUBLIC Analysis/include)
target_link_libraries(Luau.Analysis PUBLIC Luau.Ast)

target_compile_features(Luau.VM PRIVATE cxx_std_11)
target_include_directories(Luau.VM PUBLIC VM/include)

target_include_directories(isocline PUBLIC extern/isocline/include)

set(LUAU_OPTIONS)

if(MSVC)
    list(APPEND LUAU_OPTIONS /D_CRT_SECURE_NO_WARNINGS) # We need to use the portable CRT functions.
    list(APPEND LUAU_OPTIONS /MP) # Distribute single project compilation across multiple cores
else()
    list(APPEND LUAU_OPTIONS -Wall) # All warnings
endif()

if (CMAKE_CXX_COMPILER_ID STREQUAL "GNU")
    # Some gcc versions treat var in `if (type var = val)` as unused
    # Some gcc versions treat variables used in constexpr if blocks as unused
    list(APPEND LUAU_OPTIONS -Wno-unused)
endif()

# Enabled in CI; we should be warning free on our main compiler versions but don't guarantee being warning free everywhere
if(LUAU_WERROR)
    if(MSVC)
        list(APPEND LUAU_OPTIONS /WX) # Warnings are errors
    else()
        list(APPEND LUAU_OPTIONS -Werror) # Warnings are errors
    endif()
endif()

if(LUAU_BUILD_WEB)
    # add -fexceptions for emscripten to allow exceptions to be caught in C++
    list(APPEND LUAU_OPTIONS -fexceptions)
endif()

set(ISOCLINE_OPTIONS)

if (NOT MSVC)
    list(APPEND ISOCLINE_OPTIONS -Wno-unused-function)
endif()

target_compile_options(Luau.Ast PRIVATE ${LUAU_OPTIONS})
target_compile_options(Luau.Analysis PRIVATE ${LUAU_OPTIONS})
target_compile_options(Luau.VM PRIVATE ${LUAU_OPTIONS})
target_compile_options(isocline PRIVATE ${LUAU_OPTIONS} ${ISOCLINE_OPTIONS})

if (MSVC AND MSVC_VERSION GREATER_EQUAL 1924)
    # disable partial redundancy elimination which regresses interpreter codegen substantially in VS2022:
    # https://developercommunity.visualstudio.com/t/performance-regression-on-a-complex-interpreter-lo/1631863
    set_source_files_properties(VM/src/lvmexecute.cpp PROPERTIES COMPILE_FLAGS /d2ssa-pre-)
endif()

# embed .natvis inside the library debug information
if(MSVC)
    target_link_options(Luau.Ast INTERFACE /NATVIS:${CMAKE_CURRENT_SOURCE_DIR}/tools/natvis/Ast.natvis)
    target_link_options(Luau.Analysis INTERFACE /NATVIS:${CMAKE_CURRENT_SOURCE_DIR}/tools/natvis/Analysis.natvis)
    target_link_options(Luau.VM INTERFACE /NATVIS:${CMAKE_CURRENT_SOURCE_DIR}/tools/natvis/VM.natvis)
endif()

# make .natvis visible inside the solution
if(MSVC_IDE)
    target_sources(Luau.Ast PRIVATE tools/natvis/Ast.natvis)
    target_sources(Luau.Analysis PRIVATE tools/natvis/Analysis.natvis)
    target_sources(Luau.VM PRIVATE tools/natvis/VM.natvis)
endif()

if(LUAU_BUILD_CLI)
    target_compile_options(Luau.Repl.CLI PRIVATE ${LUAU_OPTIONS})
    target_compile_options(Luau.Analyze.CLI PRIVATE ${LUAU_OPTIONS})
    target_compile_options(Luau.Ast.CLI PRIVATE ${LUAU_OPTIONS})

    target_include_directories(Luau.Repl.CLI PRIVATE extern extern/isocline/include)

    target_link_libraries(Luau.Repl.CLI PRIVATE Luau.Compiler Luau.VM isocline)

    if(UNIX)
        find_library(LIBPTHREAD pthread)
        if (LIBPTHREAD)
            target_link_libraries(Luau.Repl.CLI PRIVATE pthread)
        endif()
    endif()

    target_link_libraries(Luau.Analyze.CLI PRIVATE Luau.Analysis)

    target_link_libraries(Luau.Ast.CLI PRIVATE Luau.Ast Luau.Analysis)
endif()

if(LUAU_BUILD_TESTS)
    target_compile_options(Luau.UnitTest PRIVATE ${LUAU_OPTIONS})
    target_include_directories(Luau.UnitTest PRIVATE extern)
    target_link_libraries(Luau.UnitTest PRIVATE Luau.Analysis Luau.Compiler)

    target_compile_options(Luau.Conformance PRIVATE ${LUAU_OPTIONS})
    target_include_directories(Luau.Conformance PRIVATE extern)
    target_link_libraries(Luau.Conformance PRIVATE Luau.Analysis Luau.Compiler Luau.VM)

    target_compile_options(Luau.CLI.Test PRIVATE ${LUAU_OPTIONS})
    target_include_directories(Luau.CLI.Test PRIVATE extern CLI)
    target_link_libraries(Luau.CLI.Test PRIVATE Luau.Compiler Luau.VM isocline)
    if(UNIX)
        find_library(LIBPTHREAD pthread)
        if (LIBPTHREAD)
            target_link_libraries(Luau.CLI.Test PRIVATE pthread)
        endif()
    endif()

endif()

if(LUAU_BUILD_WEB)
    target_compile_options(Luau.Web PRIVATE ${LUAU_OPTIONS})
    target_link_libraries(Luau.Web PRIVATE Luau.Compiler Luau.VM)

    # declare exported functions to emscripten
    target_link_options(Luau.Web PRIVATE -sEXPORTED_FUNCTIONS=['_executeScript'] -sEXPORTED_RUNTIME_METHODS=['ccall','cwrap'])

    # add -fexceptions for emscripten to allow exceptions to be caught in C++
    target_link_options(Luau.Web PRIVATE -fexceptions)

    # the output is a single .js file with an embedded wasm blob
    target_link_options(Luau.Web PRIVATE -sSINGLE_FILE=1)
endif()
