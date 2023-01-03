// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "ClassFixture.h"

#include "Luau/BuiltinDefinitions.h"

using std::nullopt;

namespace Luau
{

ClassFixture::ClassFixture()
{
    TypeArena& arena = typeChecker.globalTypes;
    TypeId numberType = typeChecker.numberType;

    unfreeze(arena);

    TypeId baseClassInstanceType = arena.addType(ClassType{"BaseClass", {}, nullopt, nullopt, {}, {}, "Test"});
    getMutable<ClassType>(baseClassInstanceType)->props = {
        {"BaseMethod", {makeFunction(arena, baseClassInstanceType, {numberType}, {})}},
        {"BaseField", {numberType}},
    };

    TypeId baseClassType = arena.addType(ClassType{"BaseClass", {}, nullopt, nullopt, {}, {}, "Test"});
    getMutable<ClassType>(baseClassType)->props = {
        {"StaticMethod", {makeFunction(arena, nullopt, {}, {numberType})}},
        {"Clone", {makeFunction(arena, nullopt, {baseClassInstanceType}, {baseClassInstanceType})}},
        {"New", {makeFunction(arena, nullopt, {}, {baseClassInstanceType})}},
    };
    typeChecker.globalScope->exportedTypeBindings["BaseClass"] = TypeFun{{}, baseClassInstanceType};
    addGlobalBinding(frontend, "BaseClass", baseClassType, "@test");

    TypeId childClassInstanceType = arena.addType(ClassType{"ChildClass", {}, baseClassInstanceType, nullopt, {}, {}, "Test"});

    getMutable<ClassType>(childClassInstanceType)->props = {
        {"Method", {makeFunction(arena, childClassInstanceType, {}, {typeChecker.stringType})}},
    };

    TypeId childClassType = arena.addType(ClassType{"ChildClass", {}, baseClassType, nullopt, {}, {}, "Test"});
    getMutable<ClassType>(childClassType)->props = {
        {"New", {makeFunction(arena, nullopt, {}, {childClassInstanceType})}},
    };
    typeChecker.globalScope->exportedTypeBindings["ChildClass"] = TypeFun{{}, childClassInstanceType};
    addGlobalBinding(frontend, "ChildClass", childClassType, "@test");

    TypeId grandChildInstanceType = arena.addType(ClassType{"GrandChild", {}, childClassInstanceType, nullopt, {}, {}, "Test"});

    getMutable<ClassType>(grandChildInstanceType)->props = {
        {"Method", {makeFunction(arena, grandChildInstanceType, {}, {typeChecker.stringType})}},
    };

    TypeId grandChildType = arena.addType(ClassType{"GrandChild", {}, baseClassType, nullopt, {}, {}, "Test"});
    getMutable<ClassType>(grandChildType)->props = {
        {"New", {makeFunction(arena, nullopt, {}, {grandChildInstanceType})}},
    };
    typeChecker.globalScope->exportedTypeBindings["GrandChild"] = TypeFun{{}, grandChildInstanceType};
    addGlobalBinding(frontend, "GrandChild", childClassType, "@test");

    TypeId anotherChildInstanceType = arena.addType(ClassType{"AnotherChild", {}, baseClassInstanceType, nullopt, {}, {}, "Test"});

    getMutable<ClassType>(anotherChildInstanceType)->props = {
        {"Method", {makeFunction(arena, anotherChildInstanceType, {}, {typeChecker.stringType})}},
    };

    TypeId anotherChildType = arena.addType(ClassType{"AnotherChild", {}, baseClassType, nullopt, {}, {}, "Test"});
    getMutable<ClassType>(anotherChildType)->props = {
        {"New", {makeFunction(arena, nullopt, {}, {anotherChildInstanceType})}},
    };
    typeChecker.globalScope->exportedTypeBindings["AnotherChild"] = TypeFun{{}, anotherChildInstanceType};
    addGlobalBinding(frontend, "AnotherChild", childClassType, "@test");

    TypeId unrelatedClassInstanceType = arena.addType(ClassType{"UnrelatedClass", {}, nullopt, nullopt, {}, {}, "Test"});

    TypeId unrelatedClassType = arena.addType(ClassType{"UnrelatedClass", {}, nullopt, nullopt, {}, {}, "Test"});
    getMutable<ClassType>(unrelatedClassType)->props = {
        {"New", {makeFunction(arena, nullopt, {}, {unrelatedClassInstanceType})}},
    };
    typeChecker.globalScope->exportedTypeBindings["UnrelatedClass"] = TypeFun{{}, unrelatedClassInstanceType};
    addGlobalBinding(frontend, "UnrelatedClass", unrelatedClassType, "@test");

    TypeId vector2MetaType = arena.addType(TableType{});

    TypeId vector2InstanceType = arena.addType(ClassType{"Vector2", {}, nullopt, vector2MetaType, {}, {}, "Test"});
    getMutable<ClassType>(vector2InstanceType)->props = {
        {"X", {numberType}},
        {"Y", {numberType}},
    };

    TypeId vector2Type = arena.addType(ClassType{"Vector2", {}, nullopt, nullopt, {}, {}, "Test"});
    getMutable<ClassType>(vector2Type)->props = {
        {"New", {makeFunction(arena, nullopt, {numberType, numberType}, {vector2InstanceType})}},
    };
    getMutable<TableType>(vector2MetaType)->props = {
        {"__add", {makeFunction(arena, nullopt, {vector2InstanceType, vector2InstanceType}, {vector2InstanceType})}},
    };
    typeChecker.globalScope->exportedTypeBindings["Vector2"] = TypeFun{{}, vector2InstanceType};
    addGlobalBinding(frontend, "Vector2", vector2Type, "@test");

    TypeId callableClassMetaType = arena.addType(TableType{});
    TypeId callableClassType = arena.addType(ClassType{"CallableClass", {}, nullopt, callableClassMetaType, {}, {}, "Test"});
    getMutable<TableType>(callableClassMetaType)->props = {
        {"__call", {makeFunction(arena, nullopt, {callableClassType, typeChecker.stringType}, {typeChecker.numberType})}},
    };
    typeChecker.globalScope->exportedTypeBindings["CallableClass"] = TypeFun{{}, callableClassType};

    for (const auto& [name, tf] : typeChecker.globalScope->exportedTypeBindings)
        persist(tf.type);

    freeze(arena);
}

} // namespace Luau
