# Luau.Common Sources
# Note: Until 3.19, INTERFACE targets couldn't have SOURCES property set
if(NOT ${CMAKE_VERSION} VERSION_LESS "3.19")
    target_sources(Luau.Common PRIVATE
        Common/include/Luau/Common.h
        Common/include/Luau/Bytecode.h
        Common/include/Luau/BytecodeUtils.h
        Common/include/Luau/DenseHash.h
        Common/include/Luau/ExperimentalFlags.h
    )
endif()

# Luau.Ast Sources
target_sources(Luau.Ast PRIVATE
    Ast/include/Luau/Ast.h
    Ast/include/Luau/Confusables.h
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
    Compiler/include/Luau/BytecodeBuilder.h
    Compiler/include/Luau/Compiler.h
    Compiler/include/luacode.h

    Compiler/src/BytecodeBuilder.cpp
    Compiler/src/Compiler.cpp
    Compiler/src/Builtins.cpp
    Compiler/src/BuiltinFolding.cpp
    Compiler/src/ConstantFolding.cpp
    Compiler/src/CostModel.cpp
    Compiler/src/TableShape.cpp
    Compiler/src/Types.cpp
    Compiler/src/ValueTracking.cpp
    Compiler/src/lcode.cpp
    Compiler/src/Builtins.h
    Compiler/src/BuiltinFolding.h
    Compiler/src/ConstantFolding.h
    Compiler/src/CostModel.h
    Compiler/src/TableShape.h
    Compiler/src/Types.h
    Compiler/src/ValueTracking.h
)

# Luau.Config Sources
target_sources(Luau.Config PRIVATE
    Config/include/Luau/Config.h
    Config/include/Luau/LinterConfig.h

    Config/src/Config.cpp
    Config/src/LinterConfig.cpp
)

# Luau.CodeGen Sources
target_sources(Luau.CodeGen PRIVATE
    CodeGen/include/Luau/AddressA64.h
    CodeGen/include/Luau/AssemblyBuilderA64.h
    CodeGen/include/Luau/AssemblyBuilderX64.h
    CodeGen/include/Luau/CodeAllocator.h
    CodeGen/include/Luau/CodeBlockUnwind.h
    CodeGen/include/Luau/CodeGen.h
    CodeGen/include/Luau/ConditionA64.h
    CodeGen/include/Luau/ConditionX64.h
    CodeGen/include/Luau/IrAnalysis.h
    CodeGen/include/Luau/IrBuilder.h
    CodeGen/include/Luau/IrCallWrapperX64.h
    CodeGen/include/Luau/IrDump.h
    CodeGen/include/Luau/IrData.h
    CodeGen/include/Luau/IrRegAllocX64.h
    CodeGen/include/Luau/IrUtils.h
    CodeGen/include/Luau/IrVisitUseDef.h
    CodeGen/include/Luau/Label.h
    CodeGen/include/Luau/OperandX64.h
    CodeGen/include/Luau/OptimizeConstProp.h
    CodeGen/include/Luau/OptimizeFinalX64.h
    CodeGen/include/Luau/RegisterA64.h
    CodeGen/include/Luau/RegisterX64.h
    CodeGen/include/Luau/UnwindBuilder.h
    CodeGen/include/Luau/UnwindBuilderDwarf2.h
    CodeGen/include/Luau/UnwindBuilderWin.h
    CodeGen/include/luacodegen.h

    CodeGen/src/AssemblyBuilderA64.cpp
    CodeGen/src/AssemblyBuilderX64.cpp
    CodeGen/src/CodeAllocator.cpp
    CodeGen/src/CodeBlockUnwind.cpp
    CodeGen/src/CodeGen.cpp
    CodeGen/src/CodeGenAssembly.cpp
    CodeGen/src/CodeGenUtils.cpp
    CodeGen/src/CodeGenA64.cpp
    CodeGen/src/CodeGenX64.cpp
    CodeGen/src/EmitBuiltinsX64.cpp
    CodeGen/src/EmitCommonX64.cpp
    CodeGen/src/EmitInstructionX64.cpp
    CodeGen/src/IrAnalysis.cpp
    CodeGen/src/IrBuilder.cpp
    CodeGen/src/IrCallWrapperX64.cpp
    CodeGen/src/IrDump.cpp
    CodeGen/src/IrLoweringA64.cpp
    CodeGen/src/IrLoweringX64.cpp
    CodeGen/src/IrRegAllocA64.cpp
    CodeGen/src/IrRegAllocX64.cpp
    CodeGen/src/IrTranslateBuiltins.cpp
    CodeGen/src/IrTranslation.cpp
    CodeGen/src/IrUtils.cpp
    CodeGen/src/IrValueLocationTracking.cpp
    CodeGen/src/lcodegen.cpp
    CodeGen/src/NativeState.cpp
    CodeGen/src/OptimizeConstProp.cpp
    CodeGen/src/OptimizeFinalX64.cpp
    CodeGen/src/UnwindBuilderDwarf2.cpp
    CodeGen/src/UnwindBuilderWin.cpp

    CodeGen/src/BitUtils.h
    CodeGen/src/ByteUtils.h
    CodeGen/src/CodeGenLower.h
    CodeGen/src/CodeGenUtils.h
    CodeGen/src/CodeGenA64.h
    CodeGen/src/CodeGenX64.h
    CodeGen/src/EmitBuiltinsX64.h
    CodeGen/src/EmitCommon.h
    CodeGen/src/EmitCommonA64.h
    CodeGen/src/EmitCommonX64.h
    CodeGen/src/EmitInstructionX64.h
    CodeGen/src/IrLoweringA64.h
    CodeGen/src/IrLoweringX64.h
    CodeGen/src/IrRegAllocA64.h
    CodeGen/src/IrTranslateBuiltins.h
    CodeGen/src/IrTranslation.h
    CodeGen/src/IrValueLocationTracking.h
    CodeGen/src/NativeState.h
)

# Luau.Analysis Sources
target_sources(Luau.Analysis PRIVATE
    Analysis/include/Luau/Anyification.h
    Analysis/include/Luau/ApplyTypeFunction.h
    Analysis/include/Luau/AstJsonEncoder.h
    Analysis/include/Luau/AstQuery.h
    Analysis/include/Luau/Autocomplete.h
    Analysis/include/Luau/BuiltinDefinitions.h
    Analysis/include/Luau/Cancellation.h
    Analysis/include/Luau/Clone.h
    Analysis/include/Luau/Constraint.h
    Analysis/include/Luau/ConstraintGenerator.h
    Analysis/include/Luau/ConstraintSolver.h
    Analysis/include/Luau/ControlFlow.h
    Analysis/include/Luau/DataFlowGraph.h
    Analysis/include/Luau/DcrLogger.h
    Analysis/include/Luau/Def.h
    Analysis/include/Luau/Differ.h
    Analysis/include/Luau/Documentation.h
    Analysis/include/Luau/Error.h
    Analysis/include/Luau/FileResolver.h
    Analysis/include/Luau/Frontend.h
    Analysis/include/Luau/GlobalTypes.h
    Analysis/include/Luau/InsertionOrderedMap.h
    Analysis/include/Luau/Instantiation.h
    Analysis/include/Luau/IostreamHelpers.h
    Analysis/include/Luau/JsonEmitter.h
    Analysis/include/Luau/Linter.h
    Analysis/include/Luau/LValue.h
    Analysis/include/Luau/Metamethods.h
    Analysis/include/Luau/Module.h
    Analysis/include/Luau/ModuleResolver.h
    Analysis/include/Luau/NonStrictTypeChecker.h
    Analysis/include/Luau/Normalize.h
    Analysis/include/Luau/Predicate.h
    Analysis/include/Luau/Quantify.h
    Analysis/include/Luau/RecursionCounter.h
    Analysis/include/Luau/Refinement.h
    Analysis/include/Luau/RequireTracer.h
    Analysis/include/Luau/Scope.h
    Analysis/include/Luau/Set.h
    Analysis/include/Luau/Simplify.h
    Analysis/include/Luau/Substitution.h
    Analysis/include/Luau/Subtyping.h
    Analysis/include/Luau/Symbol.h
    Analysis/include/Luau/ToDot.h
    Analysis/include/Luau/TopoSortStatements.h
    Analysis/include/Luau/ToString.h
    Analysis/include/Luau/Transpiler.h
    Analysis/include/Luau/TxnLog.h
    Analysis/include/Luau/Type.h
    Analysis/include/Luau/TypeArena.h
    Analysis/include/Luau/TypeAttach.h
    Analysis/include/Luau/TypeChecker2.h
    Analysis/include/Luau/TypeCheckLimits.h
    Analysis/include/Luau/TypedAllocator.h
    Analysis/include/Luau/TypeFamily.h
    Analysis/include/Luau/TypeFwd.h
    Analysis/include/Luau/TypeInfer.h
    Analysis/include/Luau/TypeOrPack.h
    Analysis/include/Luau/TypePack.h
    Analysis/include/Luau/TypePairHash.h
    Analysis/include/Luau/TypePath.h
    Analysis/include/Luau/TypeUtils.h
    Analysis/include/Luau/Unifiable.h
    Analysis/include/Luau/Unifier.h
    Analysis/include/Luau/Unifier2.h
    Analysis/include/Luau/UnifierSharedState.h
    Analysis/include/Luau/Variant.h
    Analysis/include/Luau/VisitType.h

    Analysis/src/Anyification.cpp
    Analysis/src/ApplyTypeFunction.cpp
    Analysis/src/AstJsonEncoder.cpp
    Analysis/src/AstQuery.cpp
    Analysis/src/Autocomplete.cpp
    Analysis/src/BuiltinDefinitions.cpp
    Analysis/src/Clone.cpp
    Analysis/src/Constraint.cpp
    Analysis/src/ConstraintGenerator.cpp
    Analysis/src/ConstraintSolver.cpp
    Analysis/src/DataFlowGraph.cpp
    Analysis/src/DcrLogger.cpp
    Analysis/src/Def.cpp
    Analysis/src/Differ.cpp
    Analysis/src/EmbeddedBuiltinDefinitions.cpp
    Analysis/src/Error.cpp
    Analysis/src/Frontend.cpp
    Analysis/src/GlobalTypes.cpp
    Analysis/src/Instantiation.cpp
    Analysis/src/IostreamHelpers.cpp
    Analysis/src/JsonEmitter.cpp
    Analysis/src/Linter.cpp
    Analysis/src/LValue.cpp
    Analysis/src/Module.cpp
    Analysis/src/NonStrictTypeChecker.cpp
    Analysis/src/Normalize.cpp
    Analysis/src/Quantify.cpp
    Analysis/src/Refinement.cpp
    Analysis/src/RequireTracer.cpp
    Analysis/src/Scope.cpp
    Analysis/src/Simplify.cpp
    Analysis/src/Substitution.cpp
    Analysis/src/Subtyping.cpp
    Analysis/src/Symbol.cpp
    Analysis/src/ToDot.cpp
    Analysis/src/TopoSortStatements.cpp
    Analysis/src/ToString.cpp
    Analysis/src/Transpiler.cpp
    Analysis/src/TxnLog.cpp
    Analysis/src/Type.cpp
    Analysis/src/TypeArena.cpp
    Analysis/src/TypeAttach.cpp
    Analysis/src/TypeChecker2.cpp
    Analysis/src/TypedAllocator.cpp
    Analysis/src/TypeFamily.cpp
    Analysis/src/TypeInfer.cpp
    Analysis/src/TypeOrPack.cpp
    Analysis/src/TypePack.cpp
    Analysis/src/TypePath.cpp
    Analysis/src/TypeUtils.cpp
    Analysis/src/Unifiable.cpp
    Analysis/src/Unifier.cpp
    Analysis/src/Unifier2.cpp
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
    VM/src/lbuffer.cpp
    VM/src/lbuflib.cpp
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
    VM/src/lbuffer.h
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
        CLI/Flags.h
        CLI/Flags.cpp
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
        CLI/Flags.h
        CLI/Flags.cpp
        CLI/Analyze.cpp)
endif()

if(TARGET Luau.Ast.CLI)
    # Luau.Ast.CLI Sources
    target_sources(Luau.Ast.CLI PRIVATE
        CLI/Ast.cpp
        CLI/FileUtils.h
        CLI/FileUtils.cpp
    )
endif()

if(TARGET Luau.UnitTest)
    # Luau.UnitTest Sources
    target_sources(Luau.UnitTest PRIVATE
        tests/AssemblyBuilderA64.test.cpp
        tests/AssemblyBuilderX64.test.cpp
        tests/AstJsonEncoder.test.cpp
        tests/AstQuery.test.cpp
        tests/AstQueryDsl.cpp
        tests/AstQueryDsl.h
        tests/AstVisitor.test.cpp
        tests/Autocomplete.test.cpp
        tests/BuiltinDefinitions.test.cpp
        tests/ClassFixture.cpp
        tests/ClassFixture.h
        tests/CodeAllocator.test.cpp
        tests/Compiler.test.cpp
        tests/Config.test.cpp
        tests/ConstraintGeneratorFixture.cpp
        tests/ConstraintGeneratorFixture.h
        tests/ConstraintSolver.test.cpp
        tests/CostModel.test.cpp
        tests/DataFlowGraph.test.cpp
        tests/DenseHash.test.cpp
        tests/DiffAsserts.cpp
        tests/DiffAsserts.h
        tests/Differ.test.cpp
        tests/Error.test.cpp
        tests/Fixture.cpp
        tests/Fixture.h
        tests/Frontend.test.cpp
        tests/InsertionOrderedMap.test.cpp
        tests/IostreamOptional.h
        tests/IrBuilder.test.cpp
        tests/IrCallWrapperX64.test.cpp
        tests/IrRegAllocX64.test.cpp
        tests/JsonEmitter.test.cpp
        tests/Lexer.test.cpp
        tests/Linter.test.cpp
        tests/LValue.test.cpp
        tests/Module.test.cpp
        tests/NonstrictMode.test.cpp
        tests/NonStrictTypeChecker.test.cpp
        tests/Normalize.test.cpp
        tests/NotNull.test.cpp
        tests/Parser.test.cpp
        tests/RegisterCallbacks.cpp
        tests/RegisterCallbacks.h
        tests/RequireTracer.test.cpp
        tests/RuntimeLimits.test.cpp
        tests/ScopedFlags.h
        tests/Simplify.test.cpp
        tests/Set.test.cpp
        tests/StringUtils.test.cpp
        tests/Subtyping.test.cpp
        tests/Symbol.test.cpp
        tests/ToDot.test.cpp
        tests/TopoSort.test.cpp
        tests/ToString.test.cpp
        tests/Transpiler.test.cpp
        tests/TxnLog.test.cpp
        tests/TypeFamily.test.cpp
        tests/TypeInfer.aliases.test.cpp
        tests/TypeInfer.annotations.test.cpp
        tests/TypeInfer.anyerror.test.cpp
        tests/TypeInfer.builtins.test.cpp
        tests/TypeInfer.cfa.test.cpp
        tests/TypeInfer.classes.test.cpp
        tests/TypeInfer.definitions.test.cpp
        tests/TypeInfer.functions.test.cpp
        tests/TypeInfer.generics.test.cpp
        tests/TypeInfer.intersectionTypes.test.cpp
        tests/TypeInfer.loops.test.cpp
        tests/TypeInfer.modules.test.cpp
        tests/TypeInfer.negations.test.cpp
        tests/TypeInfer.oop.test.cpp
        tests/TypeInfer.operators.test.cpp
        tests/TypeInfer.primitives.test.cpp
        tests/TypeInfer.provisional.test.cpp
        tests/TypeInfer.refinements.test.cpp
        tests/TypeInfer.rwprops.test.cpp
        tests/TypeInfer.singletons.test.cpp
        tests/TypeInfer.tables.test.cpp
        tests/TypeInfer.test.cpp
        tests/TypeInfer.tryUnify.test.cpp
        tests/TypeInfer.typePacks.cpp
        tests/TypeInfer.typestates.test.cpp
        tests/TypeInfer.unionTypes.test.cpp
        tests/TypeInfer.unknownnever.test.cpp
        tests/TypePack.test.cpp
        tests/TypePath.test.cpp
        tests/TypeVar.test.cpp
        tests/Unifier2.test.cpp
        tests/Variant.test.cpp
        tests/VisitType.test.cpp
        tests/main.cpp)
endif()

if(TARGET Luau.Conformance)
    # Luau.Conformance Sources
    target_sources(Luau.Conformance PRIVATE
        tests/RegisterCallbacks.h
        tests/RegisterCallbacks.cpp
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
        CLI/Flags.h
        CLI/Flags.cpp
        CLI/Profiler.h
        CLI/Profiler.cpp
        CLI/Repl.cpp

        tests/RegisterCallbacks.h
        tests/RegisterCallbacks.cpp
        tests/Repl.test.cpp
        tests/main.cpp)
endif()

if(TARGET Luau.Web)
    # Luau.Web Sources
    target_sources(Luau.Web PRIVATE
        CLI/Web.cpp)
endif()

if(TARGET Luau.Reduce.CLI)
    # Luau.Reduce.CLI Sources
    target_sources(Luau.Reduce.CLI PRIVATE
        CLI/Reduce.cpp
        CLI/FileUtils.cpp
        CLI/FileUtils.h
    )
endif()

if(TARGET Luau.Compile.CLI)
    # Luau.Compile.CLI Sources
    target_sources(Luau.Compile.CLI PRIVATE
        CLI/FileUtils.h
        CLI/FileUtils.cpp
        CLI/Flags.h
        CLI/Flags.cpp
        CLI/Compile.cpp)
endif()
