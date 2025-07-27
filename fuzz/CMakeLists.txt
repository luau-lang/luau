# This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
if(${CMAKE_VERSION} VERSION_LESS "3.26")
    message(WARNING "Building the Luau fuzzer requires CMake version 3.26 or higher.")
    return()
endif()

include(FetchContent)

cmake_policy(SET CMP0054 NEW)
cmake_policy(SET CMP0058 NEW)
cmake_policy(SET CMP0074 NEW)
cmake_policy(SET CMP0077 NEW)
cmake_policy(SET CMP0091 NEW)

if(NOT CMAKE_CXX_COMPILER_ID STREQUAL "Clang")
    message(WARNING "Building the Luau fuzzer requires Clang to be used. AppleClang is not sufficient.")
    return()
endif()

if(NOT CMAKE_OSX_ARCHITECTURES STREQUAL "x86_64")
    message(WARNING "Building the Luau fuzzer for ARM64 is currently unsupported.")
    return()
endif()

# protobuf / std integer types vary based on platform; disable sign-compare
# warnings for portability.
set(FUZZ_COMPILE_OPTIONS ${LUAU_OPTIONS} -fsanitize=address,fuzzer -g2 -Wno-sign-compare)
set(FUZZ_LINK_OPTIONS ${LUAU_OPTIONS} -fsanitize=address,fuzzer)

FetchContent_Declare(
    ProtobufMutator
    GIT_REPOSITORY https://github.com/google/libprotobuf-mutator
    GIT_TAG 212a7be1eb08e7f9c79732d2aab9b2097085d936
    # libprotobuf-mutator unconditionally configures its examples, but this
    # doesn't actually work with how we're building Protobuf from source. This
    # patch disables configuration of the examples.
    PATCH_COMMAND
    git apply
    --reverse
    --check
    --ignore-space-change
    --ignore-whitespace
    "${CMAKE_CURRENT_SOURCE_DIR}/libprotobuf-mutator-patch.patch"
    ||
    git apply
    --ignore-space-change
    --ignore-whitespace
    "${CMAKE_CURRENT_SOURCE_DIR}/libprotobuf-mutator-patch.patch"
)

FetchContent_Declare(
    Protobuf
    GIT_REPOSITORY https://github.com/protocolbuffers/protobuf.git
    # Needs to match the Protobuf version that libprotobuf-mutator is written for, roughly.
    GIT_TAG v22.3
    GIT_SHALLOW ON

    # libprotobuf-mutator will need to be able to find this at configuration
    # time.
    OVERRIDE_FIND_PACKAGE
)

set(protobuf_BUILD_TESTS OFF)
set(protobuf_BUILD_SHARED_LIBS OFF)
# libprotobuf-mutator relies on older module support.
set(protobuf_MODULE_COMPATIBLE ON)

find_package(Protobuf CONFIG REQUIRED)

# libprotobuf-mutator happily ignores CMP0077 because of its minimum version
# requirement. To override that, we set the policy default here.
set(CMAKE_POLICY_DEFAULT_CMP0077 NEW)
set(LIB_PROTO_MUTATOR_TESTING OFF)

FetchContent_MakeAvailable(ProtobufMutator)

# This patches around the fact that find_package isn't going to set the right
# values for libprotobuf-mutator to link against protobuf libraries.
target_link_libraries(protobuf-mutator-libfuzzer protobuf::libprotobuf)
target_link_libraries(protobuf-mutator protobuf::libprotobuf)

set(LUAU_PB_DIR ${CMAKE_CURRENT_BINARY_DIR}/protobuf)
set(LUAU_PB_SOURCES ${LUAU_PB_DIR}/luau.pb.cc ${LUAU_PB_DIR}/luau.pb.h)

add_custom_command(
    OUTPUT ${LUAU_PB_SOURCES}
    COMMAND ${CMAKE_COMMAND} -E make_directory ${LUAU_PB_DIR}
    COMMAND ${protobuf_PROTOC} ${CMAKE_CURRENT_SOURCE_DIR}/luau.proto --proto_path=${CMAKE_CURRENT_SOURCE_DIR} --cpp_out=${LUAU_PB_DIR}
    DEPENDS ${protobuf_PROTOC} ${CMAKE_CURRENT_SOURCE_DIR}/luau.proto
)

add_executable(Luau.Fuzz.Proto)
target_compile_options(Luau.Fuzz.Proto PRIVATE ${FUZZ_COMPILE_OPTIONS})
target_link_options(Luau.Fuzz.Proto PRIVATE ${FUZZ_LINK_OPTIONS})
target_compile_features(Luau.Fuzz.Proto PRIVATE cxx_std_17)
target_include_directories(Luau.Fuzz.Proto PRIVATE ${LUAU_PB_DIR} ${protobufmutator_SOURCE_DIR})
target_sources(Luau.Fuzz.Proto PRIVATE ${LUAU_PB_SOURCES} proto.cpp protoprint.cpp)
target_link_libraries(Luau.Fuzz.Proto PRIVATE protobuf::libprotobuf protobuf-mutator-libfuzzer protobuf-mutator Luau.Analysis Luau.Compiler Luau.Ast Luau.Config Luau.VM Luau.CodeGen)
set_target_properties(Luau.Fuzz.Proto PROPERTIES CXX_STANDARD_REQUIRED ON CXX_EXTENSIONS OFF OUTPUT_NAME fuzz-proto)

add_executable(Luau.Fuzz.ProtoTest)
target_compile_options(Luau.Fuzz.ProtoTest PRIVATE ${FUZZ_COMPILE_OPTIONS})
target_link_options(Luau.Fuzz.ProtoTest PRIVATE ${FUZZ_LINK_OPTIONS})
target_compile_features(Luau.Fuzz.ProtoTest PRIVATE cxx_std_17)
target_include_directories(Luau.Fuzz.ProtoTest PRIVATE ${LUAU_PB_DIR} ${protobufmutator_SOURCE_DIR})
target_sources(Luau.Fuzz.ProtoTest PRIVATE ${LUAU_PB_SOURCES} prototest.cpp protoprint.cpp)
target_link_libraries(Luau.Fuzz.ProtoTest PRIVATE protobuf::libprotobuf protobuf-mutator-libfuzzer protobuf-mutator)
set_target_properties(Luau.Fuzz.ProtoTest PROPERTIES CXX_STANDARD_REQUIRED ON CXX_EXTENSIONS OFF OUTPUT_NAME fuzz-prototest)
