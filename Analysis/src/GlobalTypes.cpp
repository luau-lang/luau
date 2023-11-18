// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "Luau/GlobalTypes.h"

LUAU_FASTFLAG(LuauInitializeStringMetatableInGlobalTypes)
LUAU_FASTFLAG(LuauBufferTypeck)

namespace Luau
{

GlobalTypes::GlobalTypes(NotNull<BuiltinTypes> builtinTypes)
    : builtinTypes(builtinTypes)
{
    globalScope = std::make_shared<Scope>(globalTypes.addTypePack(TypePackVar{FreeTypePack{TypeLevel{}}}));

    globalScope->addBuiltinTypeBinding("any", TypeFun{{}, builtinTypes->anyType});
    globalScope->addBuiltinTypeBinding("nil", TypeFun{{}, builtinTypes->nilType});
    globalScope->addBuiltinTypeBinding("number", TypeFun{{}, builtinTypes->numberType});
    globalScope->addBuiltinTypeBinding("string", TypeFun{{}, builtinTypes->stringType});
    globalScope->addBuiltinTypeBinding("boolean", TypeFun{{}, builtinTypes->booleanType});
    globalScope->addBuiltinTypeBinding("thread", TypeFun{{}, builtinTypes->threadType});
    if (FFlag::LuauBufferTypeck)
        globalScope->addBuiltinTypeBinding("buffer", TypeFun{{}, builtinTypes->bufferType});
    globalScope->addBuiltinTypeBinding("unknown", TypeFun{{}, builtinTypes->unknownType});
    globalScope->addBuiltinTypeBinding("never", TypeFun{{}, builtinTypes->neverType});

    if (FFlag::LuauInitializeStringMetatableInGlobalTypes)
    {
        unfreeze(*builtinTypes->arena);
        TypeId stringMetatableTy = makeStringMetatable(builtinTypes);
        asMutable(builtinTypes->stringType)->ty.emplace<PrimitiveType>(PrimitiveType::String, stringMetatableTy);
        persist(stringMetatableTy);
        freeze(*builtinTypes->arena);
    }
}

} // namespace Luau
