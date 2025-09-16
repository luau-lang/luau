// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "ClassFixture.h"

#include "Luau/BuiltinDefinitions.h"

using std::nullopt;

namespace Luau
{

ExternTypeFixture::ExternTypeFixture(bool prepareAutocomplete)
    : BuiltinsFixture(prepareAutocomplete)
{
}

Frontend& ExternTypeFixture::getFrontend()
{
    Frontend& f = BuiltinsFixture::getFrontend();

    GlobalTypes& globals = f.globals;
    TypeArena& arena = globals.globalTypes;
    TypeId numberType = getBuiltins()->numberType;
    TypeId stringType = getBuiltins()->stringType;

    unfreeze(arena);

    TypeId connectionType = arena.addType(ExternType{"Connection", {}, nullopt, nullopt, {}, {}, "Connection", {}});

    TypeId baseClassInstanceType = arena.addType(ExternType{"BaseClass", {}, nullopt, nullopt, {}, {}, "Test", {}});
    getMutable<ExternType>(baseClassInstanceType)->props = {
        {"BaseMethod", Property::readonly(makeFunction(arena, baseClassInstanceType, {numberType}, {}))},
        {"BaseField", {numberType}},

        {"Touched", Property::readonly(connectionType)},
    };

    getMutable<ExternType>(connectionType)->props = {
        {"Connect", {makeFunction(arena, connectionType, {makeFunction(arena, nullopt, {baseClassInstanceType}, {})}, {})}}
    };

    TypeId baseClassType = arena.addType(ExternType{"BaseClass", {}, nullopt, nullopt, {}, {}, "Test", {}});
    getMutable<ExternType>(baseClassType)->props = {
        {"StaticMethod", {makeFunction(arena, nullopt, {}, {numberType})}},
        {"Clone", {makeFunction(arena, nullopt, {baseClassInstanceType}, {baseClassInstanceType})}},
        {"New", {makeFunction(arena, nullopt, {}, {baseClassInstanceType})}},
    };
    globals.globalScope->exportedTypeBindings["BaseClass"] = TypeFun{{}, baseClassInstanceType};
    addGlobalBinding(globals, "BaseClass", baseClassType, "@test");

    TypeId childClassInstanceType = arena.addType(ExternType{"ChildClass", {}, baseClassInstanceType, nullopt, {}, {}, "Test", {}});

    getMutable<ExternType>(childClassInstanceType)->props = {
        {"Method", {makeFunction(arena, childClassInstanceType, {}, {stringType})}},
    };

    TypeId childClassType = arena.addType(ExternType{"ChildClass", {}, baseClassType, nullopt, {}, {}, "Test", {}});
    getMutable<ExternType>(childClassType)->props = {
        {"New", {makeFunction(arena, nullopt, {}, {childClassInstanceType})}},
    };
    globals.globalScope->exportedTypeBindings["ChildClass"] = TypeFun{{}, childClassInstanceType};
    addGlobalBinding(globals, "ChildClass", childClassType, "@test");

    TypeId grandChildInstanceType = arena.addType(ExternType{"GrandChild", {}, childClassInstanceType, nullopt, {}, {}, "Test", {}});

    getMutable<ExternType>(grandChildInstanceType)->props = {
        {"Method", {makeFunction(arena, grandChildInstanceType, {}, {stringType})}},
    };

    TypeId grandChildType = arena.addType(ExternType{"GrandChild", {}, baseClassType, nullopt, {}, {}, "Test", {}});
    getMutable<ExternType>(grandChildType)->props = {
        {"New", {makeFunction(arena, nullopt, {}, {grandChildInstanceType})}},
    };
    globals.globalScope->exportedTypeBindings["GrandChild"] = TypeFun{{}, grandChildInstanceType};
    addGlobalBinding(globals, "GrandChild", childClassType, "@test");

    TypeId anotherChildInstanceType = arena.addType(ExternType{"AnotherChild", {}, baseClassInstanceType, nullopt, {}, {}, "Test", {}});

    getMutable<ExternType>(anotherChildInstanceType)->props = {
        {"Method", {makeFunction(arena, anotherChildInstanceType, {}, {stringType})}},
    };

    TypeId anotherChildType = arena.addType(ExternType{"AnotherChild", {}, baseClassType, nullopt, {}, {}, "Test", {}});
    getMutable<ExternType>(anotherChildType)->props = {
        {"New", {makeFunction(arena, nullopt, {}, {anotherChildInstanceType})}},
    };
    globals.globalScope->exportedTypeBindings["AnotherChild"] = TypeFun{{}, anotherChildInstanceType};
    addGlobalBinding(globals, "AnotherChild", childClassType, "@test");

    TypeId unrelatedClassInstanceType = arena.addType(ExternType{"UnrelatedClass", {}, nullopt, nullopt, {}, {}, "Test", {}});

    TypeId unrelatedClassType = arena.addType(ExternType{"UnrelatedClass", {}, nullopt, nullopt, {}, {}, "Test", {}});
    getMutable<ExternType>(unrelatedClassType)->props = {
        {"New", {makeFunction(arena, nullopt, {}, {unrelatedClassInstanceType})}},
    };
    globals.globalScope->exportedTypeBindings["UnrelatedClass"] = TypeFun{{}, unrelatedClassInstanceType};
    addGlobalBinding(globals, "UnrelatedClass", unrelatedClassType, "@test");

    TypeId vector2MetaType = arena.addType(TableType{});

    vector2InstanceType = arena.addType(ExternType{"Vector2", {}, nullopt, vector2MetaType, {}, {}, "Test", {}});
    getMutable<ExternType>(vector2InstanceType)->props = {
        {"X", {numberType}},
        {"Y", {numberType}},
    };

    vector2Type = arena.addType(ExternType{"Vector2", {}, nullopt, nullopt, {}, {}, "Test", {}});
    getMutable<ExternType>(vector2Type)->props = {
        {"New", {makeFunction(arena, nullopt, {numberType, numberType}, {vector2InstanceType})}},
    };
    getMutable<TableType>(vector2MetaType)->props = {
        {"__add", {makeFunction(arena, nullopt, {vector2InstanceType, vector2InstanceType}, {vector2InstanceType})}},
        {"__mul",
         {arena.addType(
             IntersectionType{{
                 makeFunction(arena, vector2InstanceType, {vector2InstanceType}, {vector2InstanceType}),
                 makeFunction(arena, vector2InstanceType, {getBuiltins()->numberType}, {vector2InstanceType}),
             }}
         )}}
    };
    globals.globalScope->exportedTypeBindings["Vector2"] = TypeFun{{}, vector2InstanceType};
    addGlobalBinding(globals, "Vector2", vector2Type, "@test");

    TypeId callableClassMetaType = arena.addType(TableType{});
    TypeId callableClassType = arena.addType(ExternType{"CallableClass", {}, nullopt, callableClassMetaType, {}, {}, "Test", {}});
    getMutable<TableType>(callableClassMetaType)->props = {
        {"__call", {makeFunction(arena, nullopt, {callableClassType, stringType}, {numberType})}},
    };
    globals.globalScope->exportedTypeBindings["CallableClass"] = TypeFun{{}, callableClassType};

    auto addIndexableClass = [&arena, &globals](const char* className, TypeId keyType, TypeId returnType)
    {
        TypeId indexableClassMetaType = arena.addType(TableType{});
        TypeId indexableClassType =
            arena.addType(ExternType{className, {}, nullopt, indexableClassMetaType, {}, {}, "Test", {}, TableIndexer{keyType, returnType}});
        globals.globalScope->exportedTypeBindings[className] = TypeFun{{}, indexableClassType};
    };

    // IndexableClass has a table indexer with a key type of 'number | string' and a return type of 'number'
    addIndexableClass("IndexableClass", arena.addType(Luau::UnionType{{stringType, numberType}}), numberType);
    // IndexableNumericKeyClass has a table indexer with a key type of 'number' and a return type of 'number'
    addIndexableClass("IndexableNumericKeyClass", numberType, numberType);

    // Add a confusing derived class which shares the same name internally, but has a unique alias
    TypeId duplicateBaseClassInstanceType = arena.addType(ExternType{"BaseClass", {}, baseClassInstanceType, nullopt, {}, {}, "Test", {}});

    getMutable<ExternType>(duplicateBaseClassInstanceType)->props = {
        {"Method", {makeFunction(arena, duplicateBaseClassInstanceType, {}, {stringType})}},
    };

    addGlobalBinding(globals, "confusingBaseClassInstance", duplicateBaseClassInstanceType, "@test");

    for (const auto& [name, tf] : globals.globalScope->exportedTypeBindings)
        persist(tf.type);

    freeze(arena);
    return *frontend;
}

} // namespace Luau
