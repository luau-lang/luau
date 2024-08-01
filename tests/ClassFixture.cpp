// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "ClassFixture.h"

#include "Luau/BuiltinDefinitions.h"

using std::nullopt;

namespace Luau
{

ClassFixture::ClassFixture()
{
    GlobalTypes& globals = frontend.globals;
    TypeArena& arena = globals.globalTypes;
    TypeId numberType = builtinTypes->numberType;
    TypeId stringType = builtinTypes->stringType;

    unfreeze(arena);

    TypeId connectionType = arena.addType(ClassType{"Connection", {}, nullopt, nullopt, {}, {}, "Connection", {}});

    TypeId baseClassInstanceType = arena.addType(ClassType{"BaseClass", {}, nullopt, nullopt, {}, {}, "Test", {}});
    getMutable<ClassType>(baseClassInstanceType)->props = {
        {"BaseMethod", Property::readonly(makeFunction(arena, baseClassInstanceType, {numberType}, {}))},
        {"BaseField", {numberType}},

        {"Touched", Property::readonly(connectionType)},
    };

    getMutable<ClassType>(connectionType)->props = {
        {"Connect", {makeFunction(arena, connectionType, {makeFunction(arena, nullopt, {baseClassInstanceType}, {})}, {})}}
    };

    TypeId baseClassType = arena.addType(ClassType{"BaseClass", {}, nullopt, nullopt, {}, {}, "Test", {}});
    getMutable<ClassType>(baseClassType)->props = {
        {"StaticMethod", {makeFunction(arena, nullopt, {}, {numberType})}},
        {"Clone", {makeFunction(arena, nullopt, {baseClassInstanceType}, {baseClassInstanceType})}},
        {"New", {makeFunction(arena, nullopt, {}, {baseClassInstanceType})}},
    };
    globals.globalScope->exportedTypeBindings["BaseClass"] = TypeFun{{}, baseClassInstanceType};
    addGlobalBinding(globals, "BaseClass", baseClassType, "@test");

    TypeId childClassInstanceType = arena.addType(ClassType{"ChildClass", {}, baseClassInstanceType, nullopt, {}, {}, "Test", {}});

    getMutable<ClassType>(childClassInstanceType)->props = {
        {"Method", {makeFunction(arena, childClassInstanceType, {}, {stringType})}},
    };

    TypeId childClassType = arena.addType(ClassType{"ChildClass", {}, baseClassType, nullopt, {}, {}, "Test", {}});
    getMutable<ClassType>(childClassType)->props = {
        {"New", {makeFunction(arena, nullopt, {}, {childClassInstanceType})}},
    };
    globals.globalScope->exportedTypeBindings["ChildClass"] = TypeFun{{}, childClassInstanceType};
    addGlobalBinding(globals, "ChildClass", childClassType, "@test");

    TypeId grandChildInstanceType = arena.addType(ClassType{"GrandChild", {}, childClassInstanceType, nullopt, {}, {}, "Test", {}});

    getMutable<ClassType>(grandChildInstanceType)->props = {
        {"Method", {makeFunction(arena, grandChildInstanceType, {}, {stringType})}},
    };

    TypeId grandChildType = arena.addType(ClassType{"GrandChild", {}, baseClassType, nullopt, {}, {}, "Test", {}});
    getMutable<ClassType>(grandChildType)->props = {
        {"New", {makeFunction(arena, nullopt, {}, {grandChildInstanceType})}},
    };
    globals.globalScope->exportedTypeBindings["GrandChild"] = TypeFun{{}, grandChildInstanceType};
    addGlobalBinding(globals, "GrandChild", childClassType, "@test");

    TypeId anotherChildInstanceType = arena.addType(ClassType{"AnotherChild", {}, baseClassInstanceType, nullopt, {}, {}, "Test", {}});

    getMutable<ClassType>(anotherChildInstanceType)->props = {
        {"Method", {makeFunction(arena, anotherChildInstanceType, {}, {stringType})}},
    };

    TypeId anotherChildType = arena.addType(ClassType{"AnotherChild", {}, baseClassType, nullopt, {}, {}, "Test", {}});
    getMutable<ClassType>(anotherChildType)->props = {
        {"New", {makeFunction(arena, nullopt, {}, {anotherChildInstanceType})}},
    };
    globals.globalScope->exportedTypeBindings["AnotherChild"] = TypeFun{{}, anotherChildInstanceType};
    addGlobalBinding(globals, "AnotherChild", childClassType, "@test");

    TypeId unrelatedClassInstanceType = arena.addType(ClassType{"UnrelatedClass", {}, nullopt, nullopt, {}, {}, "Test", {}});

    TypeId unrelatedClassType = arena.addType(ClassType{"UnrelatedClass", {}, nullopt, nullopt, {}, {}, "Test", {}});
    getMutable<ClassType>(unrelatedClassType)->props = {
        {"New", {makeFunction(arena, nullopt, {}, {unrelatedClassInstanceType})}},
    };
    globals.globalScope->exportedTypeBindings["UnrelatedClass"] = TypeFun{{}, unrelatedClassInstanceType};
    addGlobalBinding(globals, "UnrelatedClass", unrelatedClassType, "@test");

    TypeId vector2MetaType = arena.addType(TableType{});

    vector2InstanceType = arena.addType(ClassType{"Vector2", {}, nullopt, vector2MetaType, {}, {}, "Test", {}});
    getMutable<ClassType>(vector2InstanceType)->props = {
        {"X", {numberType}},
        {"Y", {numberType}},
    };

    vector2Type = arena.addType(ClassType{"Vector2", {}, nullopt, nullopt, {}, {}, "Test", {}});
    getMutable<ClassType>(vector2Type)->props = {
        {"New", {makeFunction(arena, nullopt, {numberType, numberType}, {vector2InstanceType})}},
    };
    getMutable<TableType>(vector2MetaType)->props = {
        {"__add", {makeFunction(arena, nullopt, {vector2InstanceType, vector2InstanceType}, {vector2InstanceType})}},
        {"__mul",
         {arena.addType(IntersectionType{{
             makeFunction(arena, vector2InstanceType, {vector2InstanceType}, {vector2InstanceType}),
             makeFunction(arena, vector2InstanceType, {builtinTypes->numberType}, {vector2InstanceType}),
         }})}}
    };
    globals.globalScope->exportedTypeBindings["Vector2"] = TypeFun{{}, vector2InstanceType};
    addGlobalBinding(globals, "Vector2", vector2Type, "@test");

    TypeId callableClassMetaType = arena.addType(TableType{});
    TypeId callableClassType = arena.addType(ClassType{"CallableClass", {}, nullopt, callableClassMetaType, {}, {}, "Test", {}});
    getMutable<TableType>(callableClassMetaType)->props = {
        {"__call", {makeFunction(arena, nullopt, {callableClassType, stringType}, {numberType})}},
    };
    globals.globalScope->exportedTypeBindings["CallableClass"] = TypeFun{{}, callableClassType};

    auto addIndexableClass = [&arena, &globals](const char* className, TypeId keyType, TypeId returnType)
    {
        TypeId indexableClassMetaType = arena.addType(TableType{});
        TypeId indexableClassType =
            arena.addType(ClassType{className, {}, nullopt, indexableClassMetaType, {}, {}, "Test", {}, TableIndexer{keyType, returnType}});
        globals.globalScope->exportedTypeBindings[className] = TypeFun{{}, indexableClassType};
    };

    // IndexableClass has a table indexer with a key type of 'number | string' and a return type of 'number'
    addIndexableClass("IndexableClass", arena.addType(Luau::UnionType{{stringType, numberType}}), numberType);
    // IndexableNumericKeyClass has a table indexer with a key type of 'number' and a return type of 'number'
    addIndexableClass("IndexableNumericKeyClass", numberType, numberType);

    for (const auto& [name, tf] : globals.globalScope->exportedTypeBindings)
        persist(tf.type);

    freeze(arena);
}

} // namespace Luau
