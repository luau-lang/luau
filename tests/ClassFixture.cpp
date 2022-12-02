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

    TypeId baseClassInstanceType = arena.addType(ClassTypeVar{"BaseClass", {}, nullopt, nullopt, {}, {}, "Test"});
    getMutable<ClassTypeVar>(baseClassInstanceType)->props = {
        {"BaseMethod", {makeFunction(arena, baseClassInstanceType, {numberType}, {})}},
        {"BaseField", {numberType}},
    };

    TypeId baseClassType = arena.addType(ClassTypeVar{"BaseClass", {}, nullopt, nullopt, {}, {}, "Test"});
    getMutable<ClassTypeVar>(baseClassType)->props = {
        {"StaticMethod", {makeFunction(arena, nullopt, {}, {numberType})}},
        {"Clone", {makeFunction(arena, nullopt, {baseClassInstanceType}, {baseClassInstanceType})}},
        {"New", {makeFunction(arena, nullopt, {}, {baseClassInstanceType})}},
    };
    typeChecker.globalScope->exportedTypeBindings["BaseClass"] = TypeFun{{}, baseClassInstanceType};
    addGlobalBinding(frontend, "BaseClass", baseClassType, "@test");

    TypeId childClassInstanceType = arena.addType(ClassTypeVar{"ChildClass", {}, baseClassInstanceType, nullopt, {}, {}, "Test"});

    getMutable<ClassTypeVar>(childClassInstanceType)->props = {
        {"Method", {makeFunction(arena, childClassInstanceType, {}, {typeChecker.stringType})}},
    };

    TypeId childClassType = arena.addType(ClassTypeVar{"ChildClass", {}, baseClassType, nullopt, {}, {}, "Test"});
    getMutable<ClassTypeVar>(childClassType)->props = {
        {"New", {makeFunction(arena, nullopt, {}, {childClassInstanceType})}},
    };
    typeChecker.globalScope->exportedTypeBindings["ChildClass"] = TypeFun{{}, childClassInstanceType};
    addGlobalBinding(frontend, "ChildClass", childClassType, "@test");

    TypeId grandChildInstanceType = arena.addType(ClassTypeVar{"GrandChild", {}, childClassInstanceType, nullopt, {}, {}, "Test"});

    getMutable<ClassTypeVar>(grandChildInstanceType)->props = {
        {"Method", {makeFunction(arena, grandChildInstanceType, {}, {typeChecker.stringType})}},
    };

    TypeId grandChildType = arena.addType(ClassTypeVar{"GrandChild", {}, baseClassType, nullopt, {}, {}, "Test"});
    getMutable<ClassTypeVar>(grandChildType)->props = {
        {"New", {makeFunction(arena, nullopt, {}, {grandChildInstanceType})}},
    };
    typeChecker.globalScope->exportedTypeBindings["GrandChild"] = TypeFun{{}, grandChildInstanceType};
    addGlobalBinding(frontend, "GrandChild", childClassType, "@test");

    TypeId anotherChildInstanceType = arena.addType(ClassTypeVar{"AnotherChild", {}, baseClassInstanceType, nullopt, {}, {}, "Test"});

    getMutable<ClassTypeVar>(anotherChildInstanceType)->props = {
        {"Method", {makeFunction(arena, anotherChildInstanceType, {}, {typeChecker.stringType})}},
    };

    TypeId anotherChildType = arena.addType(ClassTypeVar{"AnotherChild", {}, baseClassType, nullopt, {}, {}, "Test"});
    getMutable<ClassTypeVar>(anotherChildType)->props = {
        {"New", {makeFunction(arena, nullopt, {}, {anotherChildInstanceType})}},
    };
    typeChecker.globalScope->exportedTypeBindings["AnotherChild"] = TypeFun{{}, anotherChildInstanceType};
    addGlobalBinding(frontend, "AnotherChild", childClassType, "@test");

    TypeId unrelatedClassInstanceType = arena.addType(ClassTypeVar{"UnrelatedClass", {}, nullopt, nullopt, {}, {}, "Test"});

    TypeId unrelatedClassType = arena.addType(ClassTypeVar{"UnrelatedClass", {}, nullopt, nullopt, {}, {}, "Test"});
    getMutable<ClassTypeVar>(unrelatedClassType)->props = {
        {"New", {makeFunction(arena, nullopt, {}, {unrelatedClassInstanceType})}},
    };
    typeChecker.globalScope->exportedTypeBindings["UnrelatedClass"] = TypeFun{{}, unrelatedClassInstanceType};
    addGlobalBinding(frontend, "UnrelatedClass", unrelatedClassType, "@test");

    TypeId vector2MetaType = arena.addType(TableTypeVar{});

    TypeId vector2InstanceType = arena.addType(ClassTypeVar{"Vector2", {}, nullopt, vector2MetaType, {}, {}, "Test"});
    getMutable<ClassTypeVar>(vector2InstanceType)->props = {
        {"X", {numberType}},
        {"Y", {numberType}},
    };

    TypeId vector2Type = arena.addType(ClassTypeVar{"Vector2", {}, nullopt, nullopt, {}, {}, "Test"});
    getMutable<ClassTypeVar>(vector2Type)->props = {
        {"New", {makeFunction(arena, nullopt, {numberType, numberType}, {vector2InstanceType})}},
    };
    getMutable<TableTypeVar>(vector2MetaType)->props = {
        {"__add", {makeFunction(arena, nullopt, {vector2InstanceType, vector2InstanceType}, {vector2InstanceType})}},
    };
    typeChecker.globalScope->exportedTypeBindings["Vector2"] = TypeFun{{}, vector2InstanceType};
    addGlobalBinding(frontend, "Vector2", vector2Type, "@test");

    TypeId callableClassMetaType = arena.addType(TableTypeVar{});
    TypeId callableClassType = arena.addType(ClassTypeVar{"CallableClass", {}, nullopt, callableClassMetaType, {}, {}, "Test"});
    getMutable<TableTypeVar>(callableClassMetaType)->props = {
        {"__call", {makeFunction(arena, nullopt, {callableClassType, typeChecker.stringType}, {typeChecker.numberType})}},
    };
    typeChecker.globalScope->exportedTypeBindings["CallableClass"] = TypeFun{{}, callableClassType};

    for (const auto& [name, tf] : typeChecker.globalScope->exportedTypeBindings)
        persist(tf.type);

    freeze(arena);
}

} // namespace Luau
