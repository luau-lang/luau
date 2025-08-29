# This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
.SUFFIXES:
MAKEFLAGS+=-r -j8
COMMA=,

CMAKE_PATH=cmake

config=debug
protobuf=system

BUILD=build/$(config)

AST_SOURCES=$(wildcard Ast/src/*.cpp)
AST_OBJECTS=$(AST_SOURCES:%=$(BUILD)/%.o)
AST_TARGET=$(BUILD)/libluauast.a

COMPILER_SOURCES=$(wildcard Compiler/src/*.cpp)
COMPILER_OBJECTS=$(COMPILER_SOURCES:%=$(BUILD)/%.o)
COMPILER_TARGET=$(BUILD)/libluaucompiler.a

CONFIG_SOURCES=$(wildcard Config/src/*.cpp)
CONFIG_OBJECTS=$(CONFIG_SOURCES:%=$(BUILD)/%.o)
CONFIG_TARGET=$(BUILD)/libluauconfig.a

ANALYSIS_SOURCES=$(wildcard Analysis/src/*.cpp)
ANALYSIS_OBJECTS=$(ANALYSIS_SOURCES:%=$(BUILD)/%.o)
ANALYSIS_TARGET=$(BUILD)/libluauanalysis.a

EQSAT_SOURCES=$(wildcard EqSat/src/*.cpp)
EQSAT_OBJECTS=$(EQSAT_SOURCES:%=$(BUILD)/%.o)
EQSAT_TARGET=$(BUILD)/libluaueqsat.a

CODEGEN_SOURCES=$(wildcard CodeGen/src/*.cpp)
CODEGEN_OBJECTS=$(CODEGEN_SOURCES:%=$(BUILD)/%.o)
CODEGEN_TARGET=$(BUILD)/libluaucodegen.a

VM_SOURCES=$(wildcard VM/src/*.cpp)
VM_OBJECTS=$(VM_SOURCES:%=$(BUILD)/%.o)
VM_TARGET=$(BUILD)/libluauvm.a

REQUIRE_SOURCES=$(wildcard Require/Runtime/src/*.cpp)
REQUIRE_OBJECTS=$(REQUIRE_SOURCES:%=$(BUILD)/%.o)
REQUIRE_TARGET=$(BUILD)/libluaurequire.a

REQUIRENAVIGATOR_SOURCES=$(wildcard Require/Navigator/src/*.cpp)
REQUIRENAVIGATOR_OBJECTS=$(REQUIRENAVIGATOR_SOURCES:%=$(BUILD)/%.o)
REQUIRENAVIGATOR_TARGET=$(BUILD)/libluaurequirenavigator.a

ISOCLINE_SOURCES=extern/isocline/src/isocline.c
ISOCLINE_OBJECTS=$(ISOCLINE_SOURCES:%=$(BUILD)/%.o)
ISOCLINE_TARGET=$(BUILD)/libisocline.a

TESTS_SOURCES=$(wildcard tests/*.cpp) CLI/src/FileUtils.cpp CLI/src/Flags.cpp CLI/src/Profiler.cpp CLI/src/Coverage.cpp CLI/src/Repl.cpp CLI/src/ReplRequirer.cpp CLI/src/VfsNavigator.cpp
TESTS_OBJECTS=$(TESTS_SOURCES:%=$(BUILD)/%.o)
TESTS_TARGET=$(BUILD)/luau-tests

REPL_CLI_SOURCES=CLI/src/FileUtils.cpp CLI/src/Flags.cpp CLI/src/Profiler.cpp CLI/src/Coverage.cpp CLI/src/Repl.cpp CLI/src/ReplEntry.cpp CLI/src/ReplRequirer.cpp CLI/src/VfsNavigator.cpp
REPL_CLI_OBJECTS=$(REPL_CLI_SOURCES:%=$(BUILD)/%.o)
REPL_CLI_TARGET=$(BUILD)/luau

ANALYZE_CLI_SOURCES=CLI/src/FileUtils.cpp CLI/src/Flags.cpp CLI/src/Analyze.cpp CLI/src/AnalyzeRequirer.cpp CLI/src/VfsNavigator.cpp
ANALYZE_CLI_OBJECTS=$(ANALYZE_CLI_SOURCES:%=$(BUILD)/%.o)
ANALYZE_CLI_TARGET=$(BUILD)/luau-analyze

COMPILE_CLI_SOURCES=CLI/src/FileUtils.cpp CLI/src/Flags.cpp CLI/src/Compile.cpp
COMPILE_CLI_OBJECTS=$(COMPILE_CLI_SOURCES:%=$(BUILD)/%.o)
COMPILE_CLI_TARGET=$(BUILD)/luau-compile

BYTECODE_CLI_SOURCES=CLI/src/FileUtils.cpp CLI/src/Flags.cpp CLI/src/Bytecode.cpp
BYTECODE_CLI_OBJECTS=$(BYTECODE_CLI_SOURCES:%=$(BUILD)/%.o)
BYTECODE_CLI_TARGET=$(BUILD)/luau-bytecode

FUZZ_SOURCES=$(wildcard fuzz/*.cpp) fuzz/luau.pb.cpp
FUZZ_OBJECTS=$(FUZZ_SOURCES:%=$(BUILD)/%.o)

TESTS_ARGS=
ifneq ($(flags),)
	TESTS_ARGS+=--fflags=$(flags)
endif
ifneq ($(opt),)
	TESTS_ARGS+=-O$(opt)
endif

OBJECTS=$(AST_OBJECTS) $(COMPILER_OBJECTS) $(CONFIG_OBJECTS) $(ANALYSIS_OBJECTS) $(EQSAT_OBJECTS) $(CODEGEN_OBJECTS) $(VM_OBJECTS) $(REQUIRE_OBJECTS) $(REQUIRENAVIGATOR_OBJECTS) $(ISOCLINE_OBJECTS) $(TESTS_OBJECTS) $(REPL_CLI_OBJECTS) $(ANALYZE_CLI_OBJECTS) $(COMPILE_CLI_OBJECTS) $(BYTECODE_CLI_OBJECTS) $(FUZZ_OBJECTS)
EXECUTABLE_ALIASES = luau luau-analyze luau-compile luau-bytecode luau-tests

# common flags
CXXFLAGS=-g -Wall
LDFLAGS=

# some gcc versions treat var in `if (type var = val)` as unused
# some gcc versions treat variables used in constexpr if blocks as unused
# some gcc versions warn maybe uninitalized on optional<std::string> members on structs
ifeq ($(findstring g++,$(shell $(CXX) --version)),g++)
	CXXFLAGS+=-Wno-unused
	CXXFLAGS+=-Wno-maybe-uninitialized
endif

# enabled in CI; we should be warning free on our main compiler versions but don't guarantee being warning free everywhere
ifneq ($(werror),)
	CXXFLAGS+=-Werror
	CXXFLAGS+=-Wno-unused-but-set-variable
endif

# configuration-specific flags
ifeq ($(config),release)
	CXXFLAGS+=-O2 -DNDEBUG -fno-math-errno
endif

ifeq ($(config),coverage)
	CXXFLAGS+=-fprofile-instr-generate -fcoverage-mapping
	LDFLAGS+=-fprofile-instr-generate
endif

ifeq ($(config),sanitize)
	CXXFLAGS+=-fsanitize=address -O1
	LDFLAGS+=-fsanitize=address
endif

ifeq ($(config),analyze)
	CXXFLAGS+=--analyze
endif

ifeq ($(config),fuzz)
	CXXFLAGS+=-fsanitize=address,fuzzer -Ibuild/libprotobuf-mutator -O2
	LDFLAGS+=-fsanitize=address,fuzzer
	LPROTOBUF=-lprotobuf
	DPROTOBUF=-D CMAKE_BUILD_TYPE=Release -D LIB_PROTO_MUTATOR_TESTING=OFF
	EPROTOC=protoc
endif

ifeq ($(config),profile)
	CXXFLAGS+=-O2 -DNDEBUG -fno-math-errno -gdwarf-4 -DCALLGRIND=1
endif

ifeq ($(protobuf),download)
	CXXFLAGS+=-Ibuild/libprotobuf-mutator/external.protobuf/include
	LPROTOBUF=build/libprotobuf-mutator/external.protobuf/lib/libprotobuf.a
	DPROTOBUF+=-D LIB_PROTO_MUTATOR_DOWNLOAD_PROTOBUF=ON
	EPROTOC=../build/libprotobuf-mutator/external.protobuf/bin/protoc
endif

ifneq ($(native),)
	TESTS_ARGS+=--codegen
endif

ifneq ($(nativelj),)
	CXXFLAGS+=-DLUA_USE_LONGJMP=1
	TESTS_ARGS+=--codegen
endif

# target-specific flags
$(AST_OBJECTS): CXXFLAGS+=-std=c++17 -ICommon/include -IAst/include
$(COMPILER_OBJECTS): CXXFLAGS+=-std=c++17 -ICompiler/include -ICommon/include -IAst/include
$(CONFIG_OBJECTS): CXXFLAGS+=-std=c++17 -IConfig/include -ICommon/include -IAst/include
$(ANALYSIS_OBJECTS): CXXFLAGS+=-std=c++17 -ICommon/include -IAst/include -IAnalysis/include -IEqSat/include -IConfig/include -ICompiler/include -IVM/include
$(EQSAT_OBJECTS): CXXFLAGS+=-std=c++17 -ICommon/include -IEqSat/include
$(CODEGEN_OBJECTS): CXXFLAGS+=-std=c++17 -ICommon/include -ICodeGen/include -IVM/include -IVM/src # Code generation needs VM internals
$(VM_OBJECTS): CXXFLAGS+=-std=c++11 -ICommon/include -IVM/include
$(REQUIRE_OBJECTS): CXXFLAGS+=-std=c++17 -ICommon/include -IVM/include -IAst/include -IConfig/include -IRequire/Navigator/include -IRequire/Runtime/include
$(REQUIRENAVIGATOR_OBJECTS): CXXFLAGS+=-std=c++17 -ICommon/include -IAst/include -IConfig/include -IRequire/Navigator/include
$(ISOCLINE_OBJECTS): CXXFLAGS+=-Wno-unused-function -Iextern/isocline/include
$(TESTS_OBJECTS): CXXFLAGS+=-std=c++17 -ICommon/include -IAst/include -ICompiler/include -IConfig/include -IAnalysis/include -IEqSat/include -ICodeGen/include -IVM/include -IRequire/Runtime/include -ICLI/include -Iextern -DDOCTEST_CONFIG_DOUBLE_STRINGIFY
$(REPL_CLI_OBJECTS): CXXFLAGS+=-std=c++17 -ICommon/include -IAst/include -ICompiler/include -IVM/include -ICodeGen/include -IRequire/Runtime/include -Iextern -Iextern/isocline/include -ICLI/include
$(ANALYZE_CLI_OBJECTS): CXXFLAGS+=-std=c++17 -ICommon/include -IAst/include -IAnalysis/include -IEqSat/include -IConfig/include -IRequire/Navigator/include -Iextern -ICLI/include
$(COMPILE_CLI_OBJECTS): CXXFLAGS+=-std=c++17 -ICommon/include -IAst/include -ICompiler/include -IVM/include -ICodeGen/include -ICLI/include
$(BYTECODE_CLI_OBJECTS): CXXFLAGS+=-std=c++17 -ICommon/include -IAst/include -ICompiler/include -IVM/include -ICodeGen/include -ICLI/include
$(FUZZ_OBJECTS): CXXFLAGS+=-std=c++17 -ICommon/include -IAst/include -ICompiler/include -IAnalysis/include -IEqSat/include -IVM/include -ICodeGen/include -IConfig/include

$(TESTS_TARGET): LDFLAGS+=-lpthread
$(REPL_CLI_TARGET): LDFLAGS+=-lpthread
$(ANALYZE_CLI_TARGET): LDFLAGS+=-lpthread
fuzz-proto fuzz-prototest: LDFLAGS+=build/libprotobuf-mutator/src/libfuzzer/libprotobuf-mutator-libfuzzer.a build/libprotobuf-mutator/src/libprotobuf-mutator.a $(LPROTOBUF)

# pseudo targets
.PHONY: all test clean coverage format luau-size aliases

all: $(REPL_CLI_TARGET) $(ANALYZE_CLI_TARGET) $(TESTS_TARGET) aliases

aliases: $(EXECUTABLE_ALIASES)

test: $(TESTS_TARGET)
	$(TESTS_TARGET) $(TESTS_ARGS)

conformance: $(TESTS_TARGET)
	$(TESTS_TARGET) $(TESTS_ARGS) -ts=Conformance

clean:
	rm -rf $(BUILD)
	rm -rf $(EXECUTABLE_ALIASES)

coverage: $(TESTS_TARGET) $(COMPILE_CLI_TARGET)
	$(TESTS_TARGET)
	mv default.profraw tests.profraw
	$(TESTS_TARGET) --fflags=true
	mv default.profraw tests-flags.profraw
	$(TESTS_TARGET) --fflags=true,DebugLuauDeferredConstraintResolution=true
	mv default.profraw tests-dcr.profraw
	$(TESTS_TARGET) -ts=Conformance --codegen
	mv default.profraw codegen.profraw
	$(TESTS_TARGET) -ts=Conformance --codegen --fflags=true
	mv default.profraw codegen-flags.profraw
	$(COMPILE_CLI_TARGET) --codegennull --target=a64 tests/conformance
	mv default.profraw codegen-a64.profraw
	$(COMPILE_CLI_TARGET) --codegennull --target=x64 tests/conformance
	mv default.profraw codegen-x64.profraw
	llvm-profdata merge *.profraw -o default.profdata
	rm *.profraw
	llvm-cov show -format=html -show-instantiations=false -show-line-counts=true -show-region-summary=false -ignore-filename-regex=\(tests\|extern\|CLI\)/.* -output-dir=coverage --instr-profile default.profdata -object build/coverage/luau-tests -object build/coverage/luau-compile
	llvm-cov report -ignore-filename-regex=\(tests\|extern\|CLI\)/.* -show-region-summary=false --instr-profile default.profdata -object build/coverage/luau-tests -object build/coverage/luau-compile
	llvm-cov export -ignore-filename-regex=\(tests\|extern\|CLI\)/.* -format lcov --instr-profile default.profdata -object build/coverage/luau-tests -object build/coverage/luau-compile >coverage.info

format:
	git ls-files '*.h' '*.cpp' | xargs clang-format-11 -i

luau-size: luau
	nm --print-size --demangle luau | grep ' t void luau_execute<false>' | awk -F ' ' '{sum += strtonum("0x" $$2)} END {print sum " interpreter" }'
	nm --print-size --demangle luau | grep ' t luauF_' | awk -F ' ' '{sum += strtonum("0x" $$2)} END {print sum " builtins" }'

check-source:
	git ls-files '*.h' '*.cpp' | xargs -I+ sh -c 'grep -L LICENSE +'
	git ls-files '*.h' ':!:extern' | xargs -I+ sh -c 'grep -L "#pragma once" +'

# executable target aliases
luau: $(REPL_CLI_TARGET)
	ln -fs $^ $@

luau-analyze: $(ANALYZE_CLI_TARGET)
	ln -fs $^ $@

luau-compile: $(COMPILE_CLI_TARGET)
	ln -fs $^ $@

luau-bytecode: $(BYTECODE_CLI_TARGET)
	ln -fs $^ $@

luau-tests: $(TESTS_TARGET)
	ln -fs $^ $@

# executable targets
$(TESTS_TARGET): $(TESTS_OBJECTS) $(ANALYSIS_TARGET) $(EQSAT_TARGET) $(COMPILER_TARGET) $(AST_TARGET) $(CODEGEN_TARGET) $(VM_TARGET) $(REQUIRE_TARGET) $(REQUIRENAVIGATOR_TARGET) $(CONFIG_TARGET) $(ISOCLINE_TARGET)
$(REPL_CLI_TARGET): $(REPL_CLI_OBJECTS) $(COMPILER_TARGET) $(AST_TARGET) $(CODEGEN_TARGET) $(VM_TARGET) $(REQUIRE_TARGET) $(REQUIRENAVIGATOR_TARGET) $(CONFIG_TARGET) $(ISOCLINE_TARGET)
$(ANALYZE_CLI_TARGET): $(ANALYZE_CLI_OBJECTS) $(ANALYSIS_TARGET) $(EQSAT_TARGET) $(AST_TARGET) $(COMPILER_TARGET) $(VM_TARGET) $(REQUIRENAVIGATOR_TARGET) $(CONFIG_TARGET)
$(COMPILE_CLI_TARGET): $(COMPILE_CLI_OBJECTS) $(COMPILER_TARGET) $(AST_TARGET) $(CODEGEN_TARGET) $(VM_TARGET)
$(BYTECODE_CLI_TARGET): $(BYTECODE_CLI_OBJECTS) $(COMPILER_TARGET) $(AST_TARGET) $(CODEGEN_TARGET) $(VM_TARGET)

$(TESTS_TARGET) $(REPL_CLI_TARGET) $(ANALYZE_CLI_TARGET) $(COMPILE_CLI_TARGET) $(BYTECODE_CLI_TARGET):
	$(CXX) $^ $(LDFLAGS) -o $@

# executable targets for fuzzing
fuzz-%: $(BUILD)/fuzz/%.cpp.o $(ANALYSIS_TARGET) $(EQSAT_TARGET) $(COMPILER_TARGET) $(AST_TARGET) $(CONFIG_TARGET) $(CODEGEN_TARGET) $(VM_TARGET)
	$(CXX) $^ $(LDFLAGS) -o $@

fuzz-proto: $(BUILD)/fuzz/proto.cpp.o $(BUILD)/fuzz/protoprint.cpp.o $(BUILD)/fuzz/luau.pb.cpp.o $(ANALYSIS_TARGET) $(EQSAT_TARGET) $(COMPILER_TARGET) $(AST_TARGET) $(CONFIG_TARGET) $(VM_TARGET) | build/libprotobuf-mutator
fuzz-prototest: $(BUILD)/fuzz/prototest.cpp.o $(BUILD)/fuzz/protoprint.cpp.o $(BUILD)/fuzz/luau.pb.cpp.o $(ANALYSIS_TARGET) $(EQSAT_TARGET) $(COMPILER_TARGET) $(AST_TARGET) $(CONFIG_TARGET) $(VM_TARGET) | build/libprotobuf-mutator

# static library targets
$(AST_TARGET): $(AST_OBJECTS)
$(COMPILER_TARGET): $(COMPILER_OBJECTS)
$(CONFIG_TARGET): $(CONFIG_OBJECTS)
$(ANALYSIS_TARGET): $(ANALYSIS_OBJECTS)
$(EQSAT_TARGET): $(EQSAT_OBJECTS)
$(CODEGEN_TARGET): $(CODEGEN_OBJECTS)
$(VM_TARGET): $(VM_OBJECTS)
$(REQUIRE_TARGET): $(REQUIRE_OBJECTS)
$(REQUIRENAVIGATOR_TARGET): $(REQUIRENAVIGATOR_OBJECTS)
$(ISOCLINE_TARGET): $(ISOCLINE_OBJECTS)

$(AST_TARGET) $(COMPILER_TARGET) $(CONFIG_TARGET) $(ANALYSIS_TARGET) $(EQSAT_TARGET) $(CODEGEN_TARGET) $(VM_TARGET) $(REQUIRE_TARGET) $(REQUIRENAVIGATOR_TARGET) $(ISOCLINE_TARGET):
	ar rcs $@ $^

# object file targets
$(BUILD)/%.cpp.o: %.cpp
	@mkdir -p $(dir $@)
	$(CXX) $< $(CXXFLAGS) -c -MMD -MP -o $@

$(BUILD)/%.c.o: %.c
	@mkdir -p $(dir $@)
	$(CXX) -x c $< $(CXXFLAGS) -c -MMD -MP -o $@

# protobuf fuzzer setup
fuzz/luau.pb.cpp: fuzz/luau.proto build/libprotobuf-mutator
	cd fuzz && $(EPROTOC) luau.proto --cpp_out=.
	mv fuzz/luau.pb.cc fuzz/luau.pb.cpp

$(BUILD)/fuzz/proto.cpp.o: fuzz/luau.pb.cpp
$(BUILD)/fuzz/protoprint.cpp.o: fuzz/luau.pb.cpp
$(BUILD)/fuzz/prototest.cpp.o: fuzz/luau.pb.cpp

build/libprotobuf-mutator:
	git clone https://github.com/google/libprotobuf-mutator build/libprotobuf-mutator
	git -C build/libprotobuf-mutator checkout 212a7be1eb08e7f9c79732d2aab9b2097085d936
	$(CMAKE_PATH) -DCMAKE_CXX_COMPILER=$(CMAKE_CXX) -DCMAKE_C_COMPILER=$(CMAKE_CC) -DCMAKE_CXX_COMPILER_LAUNCHER=$(CMAKE_PROXY) -S build/libprotobuf-mutator -B build/libprotobuf-mutator $(DPROTOBUF)
	$(MAKE) -C build/libprotobuf-mutator

# picks up include dependencies for all object files
-include $(OBJECTS:.o=.d)
