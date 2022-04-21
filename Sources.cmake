# Luau.Ast Sources
target_sources(Luau.Ast PRIVATE
    Ast/include/Luau/Ast.h
    Ast/include/Luau/Common.h
    Ast/include/Luau/Confusables.h
    Ast/include/Luau/DenseHash.h
    Ast/include/Luau/Lexer.h
    Ast/include/Luau/Location.h
    Ast/include/Luau/ParseOptions.h
    Ast/include/Luau/Parser.h
    Ast/include/Luau/ParseResult.h
    Ast/include/Luau/StringUtils.h
    Ast/include/Luau/TimeTrace.h

    Ast/src/Ast.cpp
    Ast/src/Confusables.cpp
    Ast/src/Lexer.cpp
    Ast/src/Location.cpp
    Ast/src/Parser.cpp
    Ast/src/StringUtils.cpp
    Ast/src/TimeTrace.cpp
)

# Luau.Compiler Sources
target_sources(Luau.Compiler PRIVATE
    Compiler/include/Luau/Bytecode.h
    Compiler/include/Luau/BytecodeBuilder.h
    Compiler/include/Luau/Compiler.h
    Compiler/include/luacode.h

    Compiler/src/BytecodeBuilder.cpp
    Compiler/src/Compiler.cpp
    Compiler/src/Builtins.cpp
    Compiler/src/ConstantFolding.cpp
    Compiler/src/CostModel.cpp
    Compiler/src/TableShape.cpp
    Compiler/src/ValueTracking.cpp
    Compiler/src/lcode.cpp
    Compiler/src/Builtins.h
    Compiler/src/ConstantFolding.h
    Compiler/src/CostModel.h
    Compiler/src/TableShape.h
    Compiler/src/ValueTracking.h
)

# Luau.Analysis Sources
target_sources(Luau.Analysis PRIVATE
    Analysis/include/Luau/AstQuery.h
    Analysis/include/Luau/Autocomplete.h
    Analysis/include/Luau/BuiltinDefinitions.h
    Analysis/include/Luau/Config.h
    Analysis/include/Luau/Clone.h
    Analysis/include/Luau/Documentation.h
    Analysis/include/Luau/Error.h
    Analysis/include/Luau/FileResolver.h
    Analysis/include/Luau/Frontend.h
    Analysis/include/Luau/IostreamHelpers.h
    Analysis/include/Luau/JsonEncoder.h
    Analysis/include/Luau/Linter.h
    Analysis/include/Luau/LValue.h
    Analysis/include/Luau/Module.h
    Analysis/include/Luau/ModuleResolver.h
    Analysis/include/Luau/Normalize.h
    Analysis/include/Luau/Predicate.h
    Analysis/include/Luau/Quantify.h
    Analysis/include/Luau/RecursionCounter.h
    Analysis/include/Luau/RequireTracer.h
    Analysis/include/Luau/Scope.h
    Analysis/include/Luau/Substitution.h
    Analysis/include/Luau/Symbol.h
    Analysis/include/Luau/ToDot.h
    Analysis/include/Luau/TopoSortStatements.h
    Analysis/include/Luau/ToString.h
    Analysis/include/Luau/Transpiler.h
    Analysis/include/Luau/TxnLog.h
    Analysis/include/Luau/TypeAttach.h
    Analysis/include/Luau/TypedAllocator.h
    Analysis/include/Luau/TypeInfer.h
    Analysis/include/Luau/TypePack.h
    Analysis/include/Luau/TypeUtils.h
    Analysis/include/Luau/TypeVar.h
    Analysis/include/Luau/Unifiable.h
    Analysis/include/Luau/Unifier.h
    Analysis/include/Luau/UnifierSharedState.h
    Analysis/include/Luau/Variant.h
    Analysis/include/Luau/VisitTypeVar.h

    Analysis/src/AstQuery.cpp
    Analysis/src/Autocomplete.cpp
    Analysis/src/BuiltinDefinitions.cpp
    Analysis/src/Config.cpp
    Analysis/src/Clone.cpp
    Analysis/src/Error.cpp
    Analysis/src/Frontend.cpp
    Analysis/src/IostreamHelpers.cpp
    Analysis/src/JsonEncoder.cpp
    Analysis/src/Linter.cpp
    Analysis/src/LValue.cpp
    Analysis/src/Module.cpp
    Analysis/src/Normalize.cpp
    Analysis/src/Quantify.cpp
    Analysis/src/RequireTracer.cpp
    Analysis/src/Scope.cpp
    Analysis/src/Substitution.cpp
    Analysis/src/Symbol.cpp
    Analysis/src/ToDot.cpp
    Analysis/src/TopoSortStatements.cpp
    Analysis/src/ToString.cpp
    Analysis/src/Transpiler.cpp
    Analysis/src/TxnLog.cpp
    Analysis/src/TypeAttach.cpp
    Analysis/src/TypedAllocator.cpp
    Analysis/src/TypeInfer.cpp
    Analysis/src/TypePack.cpp
    Analysis/src/TypeUtils.cpp
    Analysis/src/TypeVar.cpp
    Analysis/src/Unifiable.cpp
    Analysis/src/Unifier.cpp
    Analysis/src/EmbeddedBuiltinDefinitions.cpp
)

# Luau.VM Sources
target_sources(Luau.VM PRIVATE
    VM/include/lua.h
    VM/include/luaconf.h
    VM/include/lualib.h

    VM/src/lapi.cpp
    VM/src/laux.cpp
    VM/src/lbaselib.cpp
    VM/src/lbitlib.cpp
    VM/src/lbuiltins.cpp
    VM/src/lcorolib.cpp
    VM/src/ldblib.cpp
    VM/src/ldebug.cpp
    VM/src/ldo.cpp
    VM/src/lfunc.cpp
    VM/src/lgc.cpp
    VM/src/lgcdebug.cpp
    VM/src/linit.cpp
    VM/src/lmathlib.cpp
    VM/src/lmem.cpp
    VM/src/lnumprint.cpp
    VM/src/lobject.cpp
    VM/src/loslib.cpp
    VM/src/lperf.cpp
    VM/src/lstate.cpp
    VM/src/lstring.cpp
    VM/src/lstrlib.cpp
    VM/src/ltable.cpp
    VM/src/ltablib.cpp
    VM/src/ltm.cpp
    VM/src/ludata.cpp
    VM/src/lutf8lib.cpp
    VM/src/lvmexecute.cpp
    VM/src/lvmload.cpp
    VM/src/lvmutils.cpp
    VM/src/lapi.h
    VM/src/lbuiltins.h
    VM/src/lbytecode.h
    VM/src/lcommon.h
    VM/src/ldebug.h
    VM/src/ldo.h
    VM/src/lfunc.h
    VM/src/lgc.h
    VM/src/lmem.h
    VM/src/lnumutils.h
    VM/src/lobject.h
    VM/src/lstate.h
    VM/src/lstring.h
    VM/src/ltable.h
    VM/src/ltm.h
    VM/src/ludata.h
    VM/src/lvm.h
)

target_sources(isocline PRIVATE
    extern/isocline/include/isocline.h
    extern/isocline/src/isocline.c
)

if(TARGET Luau.Repl.CLI)
    # Luau.Repl.CLI Sources
    target_sources(Luau.Repl.CLI PRIVATE
        CLI/Coverage.h
        CLI/Coverage.cpp
        CLI/FileUtils.h
        CLI/FileUtils.cpp
        CLI/Profiler.h
        CLI/Profiler.cpp
        CLI/Repl.cpp
        CLI/ReplEntry.cpp)
endif()

if(TARGET Luau.Analyze.CLI)
    # Luau.Analyze.CLI Sources
    target_sources(Luau.Analyze.CLI PRIVATE
        CLI/FileUtils.h
        CLI/FileUtils.cpp
        CLI/Analyze.cpp)
endif()

if(TARGET Luau.Ast.CLI)
    target_sources(Luau.Ast.CLI PRIVATE
        CLI/Ast.cpp
        CLI/FileUtils.h
        CLI/FileUtils.cpp
    )
endif()

if(TARGET Luau.UnitTest)
    # Luau.UnitTest Sources
    target_sources(Luau.UnitTest PRIVATE
        tests/Fixture.h
        tests/IostreamOptional.h
        tests/ScopedFlags.h
        tests/Fixture.cpp
        tests/AstQuery.test.cpp
        tests/AstVisitor.test.cpp
        tests/Autocomplete.test.cpp
        tests/BuiltinDefinitions.test.cpp
        tests/Compiler.test.cpp
        tests/CostModel.test.cpp
        tests/Config.test.cpp
        tests/Error.test.cpp
        tests/Frontend.test.cpp
        tests/JsonEncoder.test.cpp
        tests/Linter.test.cpp
        tests/LValue.test.cpp
        tests/Module.test.cpp
        tests/NonstrictMode.test.cpp
        tests/Normalize.test.cpp
        tests/Parser.test.cpp
        tests/RequireTracer.test.cpp
        tests/StringUtils.test.cpp
        tests/Symbol.test.cpp
        tests/ToDot.test.cpp
        tests/TopoSort.test.cpp
        tests/ToString.test.cpp
        tests/Transpiler.test.cpp
        tests/TypeInfer.aliases.test.cpp
        tests/TypeInfer.annotations.test.cpp
        tests/TypeInfer.anyerror.test.cpp
        tests/TypeInfer.builtins.test.cpp
        tests/TypeInfer.classes.test.cpp
        tests/TypeInfer.definitions.test.cpp
        tests/TypeInfer.functions.test.cpp
        tests/TypeInfer.generics.test.cpp
        tests/TypeInfer.intersectionTypes.test.cpp
        tests/TypeInfer.loops.test.cpp
        tests/TypeInfer.modules.test.cpp
        tests/TypeInfer.oop.test.cpp
        tests/TypeInfer.operators.test.cpp
        tests/TypeInfer.primitives.test.cpp
        tests/TypeInfer.provisional.test.cpp
        tests/TypeInfer.refinements.test.cpp
        tests/TypeInfer.singletons.test.cpp
        tests/TypeInfer.tables.test.cpp
        tests/TypeInfer.test.cpp
        tests/TypeInfer.tryUnify.test.cpp
        tests/TypeInfer.typePacks.cpp
        tests/TypeInfer.unionTypes.test.cpp
        tests/TypePack.test.cpp
        tests/TypeVar.test.cpp
        tests/Variant.test.cpp
        tests/main.cpp)
endif()

if(TARGET Luau.Conformance)
    # Luau.Conformance Sources
    target_sources(Luau.Conformance PRIVATE
        tests/Conformance.test.cpp
        tests/main.cpp)
endif()

if(TARGET Luau.CLI.Test)
    # Luau.CLI.Test Sources
    target_sources(Luau.CLI.Test PRIVATE
        CLI/Coverage.h
        CLI/Coverage.cpp
        CLI/FileUtils.h
        CLI/FileUtils.cpp
        CLI/Profiler.h
        CLI/Profiler.cpp
        CLI/Repl.cpp
    
        tests/Repl.test.cpp
        tests/main.cpp)
endif()

if(TARGET Luau.Web)
    # Luau.Web Sources
    target_sources(Luau.Web PRIVATE
        CLI/Web.cpp)
endif()
