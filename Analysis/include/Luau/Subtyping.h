// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/Set.h"
#include "Luau/TypeFwd.h"
#include "Luau/TypePairHash.h"
#include "Luau/TypePath.h"
#include "Luau/TypeFunction.h"
#include "Luau/TypeCheckLimits.h"
#include "Luau/DenseHash.h"

#include <vector>
#include <optional>

namespace Luau
{

template<typename A, typename B>
struct TryPair;
struct InternalErrorReporter;

class TypeIds;
class Normalizer;
struct NormalizedClassType;
struct NormalizedFunctionType;
struct NormalizedStringType;
struct NormalizedType;
struct Property;
struct Scope;
struct TableIndexer;
struct TypeArena;
struct TypeCheckLimits;

enum class SubtypingVariance
{
    // Used for an empty key. Should never appear in actual code.
    Invalid,
    Covariant,
    // This is used to identify cases where we have a covariant + a
    // contravariant reason and we need to merge them.
    Contravariant,
    Invariant,
};

struct SubtypingReasoning
{
    // The path, relative to the _root subtype_, where subtyping failed.
    Path subPath;
    // The path, relative to the _root supertype_, where subtyping failed.
    Path superPath;
    SubtypingVariance variance = SubtypingVariance::Covariant;

    bool operator==(const SubtypingReasoning& other) const;
};

struct SubtypingReasoningHash
{
    size_t operator()(const SubtypingReasoning& r) const;
};

using SubtypingReasonings = DenseHashSet<SubtypingReasoning, SubtypingReasoningHash>;
static const SubtypingReasoning kEmptyReasoning = SubtypingReasoning{TypePath::kEmpty, TypePath::kEmpty, SubtypingVariance::Invalid};

struct SubtypingResult
{
    bool isSubtype = false;
    bool normalizationTooComplex = false;
    bool isCacheable = true;
    ErrorVec errors;
    /// The reason for isSubtype to be false. May not be present even if
    /// isSubtype is false, depending on the input types.
    SubtypingReasonings reasoning{kEmptyReasoning};

    SubtypingResult& andAlso(const SubtypingResult& other);
    SubtypingResult& orElse(const SubtypingResult& other);
    SubtypingResult& withBothComponent(TypePath::Component component);
    SubtypingResult& withSuperComponent(TypePath::Component component);
    SubtypingResult& withSubComponent(TypePath::Component component);
    SubtypingResult& withBothPath(TypePath::Path path);
    SubtypingResult& withSubPath(TypePath::Path path);
    SubtypingResult& withSuperPath(TypePath::Path path);
    SubtypingResult& withErrors(ErrorVec& err);
    SubtypingResult& withError(TypeError err);

    // Only negates the `isSubtype`.
    static SubtypingResult negate(const SubtypingResult& result);
    static SubtypingResult all(const std::vector<SubtypingResult>& results);
    static SubtypingResult any(const std::vector<SubtypingResult>& results);
};

struct SubtypingEnvironment
{
    struct GenericBounds
    {
        DenseHashSet<TypeId> lowerBound{nullptr};
        DenseHashSet<TypeId> upperBound{nullptr};
    };

    /*
     * When we encounter a generic over the course of a subtyping test, we need
     * to tentatively map that generic onto a type on the other side.
     */
    DenseHashMap<TypeId, GenericBounds> mappedGenerics{nullptr};
    DenseHashMap<TypePackId, TypePackId> mappedGenericPacks{nullptr};

    /*
     * See the test cyclic_tables_are_assumed_to_be_compatible_with_classes for
     * details.
     *
     * An empty value is equivalent to a nonexistent key.
     */
    DenseHashMap<TypeId, TypeId> substitutions{nullptr};

    DenseHashMap<std::pair<TypeId, TypeId>, SubtypingResult, TypePairHash> ephemeralCache{{}};

    /// Applies `mappedGenerics` to the given type.
    /// This is used specifically to substitute for generics in type function instances.
    std::optional<TypeId> applyMappedGenerics(NotNull<BuiltinTypes> builtinTypes, NotNull<TypeArena> arena, TypeId ty);
};

struct Subtyping
{
    NotNull<BuiltinTypes> builtinTypes;
    NotNull<TypeArena> arena;
    NotNull<Normalizer> normalizer;
    NotNull<InternalErrorReporter> iceReporter;

    NotNull<Scope> scope;
    TypeCheckLimits limits;

    enum class Variance
    {
        Covariant,
        Contravariant
    };

    Variance variance = Variance::Covariant;

    using SeenSet = Set<std::pair<TypeId, TypeId>, TypePairHash>;

    SeenSet seenTypes{{}};

    Subtyping(NotNull<BuiltinTypes> builtinTypes, NotNull<TypeArena> typeArena, NotNull<Normalizer> normalizer,
        NotNull<InternalErrorReporter> iceReporter, NotNull<Scope> scope);

    Subtyping(const Subtyping&) = delete;
    Subtyping& operator=(const Subtyping&) = delete;

    Subtyping(Subtyping&&) = default;
    Subtyping& operator=(Subtyping&&) = default;

    // Only used by unit tests to test that the cache works.
    const DenseHashMap<std::pair<TypeId, TypeId>, SubtypingResult, TypePairHash>& peekCache() const
    {
        return resultCache;
    }

    // TODO cache
    // TODO cyclic types
    // TODO recursion limits

    SubtypingResult isSubtype(TypeId subTy, TypeId superTy);
    SubtypingResult isSubtype(TypePackId subTy, TypePackId superTy);

private:
    DenseHashMap<std::pair<TypeId, TypeId>, SubtypingResult, TypePairHash> resultCache{{}};

    SubtypingResult cache(SubtypingEnvironment& env, SubtypingResult res, TypeId subTy, TypeId superTy);

    SubtypingResult isCovariantWith(SubtypingEnvironment& env, TypeId subTy, TypeId superTy);
    SubtypingResult isCovariantWith(SubtypingEnvironment& env, TypePackId subTy, TypePackId superTy);

    template<typename SubTy, typename SuperTy>
    SubtypingResult isContravariantWith(SubtypingEnvironment& env, SubTy&& subTy, SuperTy&& superTy);

    template<typename SubTy, typename SuperTy>
    SubtypingResult isInvariantWith(SubtypingEnvironment& env, SubTy&& subTy, SuperTy&& superTy);

    template<typename SubTy, typename SuperTy>
    SubtypingResult isCovariantWith(SubtypingEnvironment& env, const TryPair<const SubTy*, const SuperTy*>& pair);

    template<typename SubTy, typename SuperTy>
    SubtypingResult isContravariantWith(SubtypingEnvironment& env, const TryPair<const SubTy*, const SuperTy*>& pair);

    template<typename SubTy, typename SuperTy>
    SubtypingResult isInvariantWith(SubtypingEnvironment& env, const TryPair<const SubTy*, const SuperTy*>& pair);

    SubtypingResult isCovariantWith(SubtypingEnvironment& env, TypeId subTy, const UnionType* superUnion);
    SubtypingResult isCovariantWith(SubtypingEnvironment& env, const UnionType* subUnion, TypeId superTy);
    SubtypingResult isCovariantWith(SubtypingEnvironment& env, TypeId subTy, const IntersectionType* superIntersection);
    SubtypingResult isCovariantWith(SubtypingEnvironment& env, const IntersectionType* subIntersection, TypeId superTy);

    SubtypingResult isCovariantWith(SubtypingEnvironment& env, const NegationType* subNegation, TypeId superTy);
    SubtypingResult isCovariantWith(SubtypingEnvironment& env, const TypeId subTy, const NegationType* superNegation);

    SubtypingResult isCovariantWith(SubtypingEnvironment& env, const PrimitiveType* subPrim, const PrimitiveType* superPrim);
    SubtypingResult isCovariantWith(SubtypingEnvironment& env, const SingletonType* subSingleton, const PrimitiveType* superPrim);
    SubtypingResult isCovariantWith(SubtypingEnvironment& env, const SingletonType* subSingleton, const SingletonType* superSingleton);
    SubtypingResult isCovariantWith(SubtypingEnvironment& env, const TableType* subTable, const TableType* superTable);
    SubtypingResult isCovariantWith(SubtypingEnvironment& env, const MetatableType* subMt, const MetatableType* superMt);
    SubtypingResult isCovariantWith(SubtypingEnvironment& env, const MetatableType* subMt, const TableType* superTable);
    SubtypingResult isCovariantWith(SubtypingEnvironment& env, const ClassType* subClass, const ClassType* superClass);
    SubtypingResult isCovariantWith(SubtypingEnvironment& env, TypeId subTy, const ClassType* subClass, TypeId superTy, const TableType* superTable);
    SubtypingResult isCovariantWith(SubtypingEnvironment& env, const FunctionType* subFunction, const FunctionType* superFunction);
    SubtypingResult isCovariantWith(SubtypingEnvironment& env, const PrimitiveType* subPrim, const TableType* superTable);
    SubtypingResult isCovariantWith(SubtypingEnvironment& env, const SingletonType* subSingleton, const TableType* superTable);

    SubtypingResult isCovariantWith(SubtypingEnvironment& env, const TableIndexer& subIndexer, const TableIndexer& superIndexer);
    SubtypingResult isCovariantWith(SubtypingEnvironment& env, const Property& subProperty, const Property& superProperty, const std::string& name);

    SubtypingResult isCovariantWith(
        SubtypingEnvironment& env, const std::shared_ptr<const NormalizedType>& subNorm, const std::shared_ptr<const NormalizedType>& superNorm);
    SubtypingResult isCovariantWith(SubtypingEnvironment& env, const NormalizedClassType& subClass, const NormalizedClassType& superClass);
    SubtypingResult isCovariantWith(SubtypingEnvironment& env, const NormalizedClassType& subClass, const TypeIds& superTables);
    SubtypingResult isCovariantWith(SubtypingEnvironment& env, const NormalizedStringType& subString, const NormalizedStringType& superString);
    SubtypingResult isCovariantWith(SubtypingEnvironment& env, const NormalizedStringType& subString, const TypeIds& superTables);
    SubtypingResult isCovariantWith(
        SubtypingEnvironment& env, const NormalizedFunctionType& subFunction, const NormalizedFunctionType& superFunction);
    SubtypingResult isCovariantWith(SubtypingEnvironment& env, const TypeIds& subTypes, const TypeIds& superTypes);

    SubtypingResult isCovariantWith(SubtypingEnvironment& env, const VariadicTypePack* subVariadic, const VariadicTypePack* superVariadic);
    SubtypingResult isCovariantWith(SubtypingEnvironment& env, const TypeFunctionInstanceType* subFunctionInstance, const TypeId superTy);
    SubtypingResult isCovariantWith(SubtypingEnvironment& env, const TypeId subTy, const TypeFunctionInstanceType* superFunctionInstance);

    bool bindGeneric(SubtypingEnvironment& env, TypeId subTp, TypeId superTp);
    bool bindGeneric(SubtypingEnvironment& env, TypePackId subTp, TypePackId superTp);

    template<typename T, typename Container>
    TypeId makeAggregateType(const Container& container, TypeId orElse);

    std::pair<TypeId, ErrorVec> handleTypeFunctionReductionResult(const TypeFunctionInstanceType* functionInstance);

    [[noreturn]] void unexpected(TypeId ty);
    [[noreturn]] void unexpected(TypePackId tp);
};

} // namespace Luau
