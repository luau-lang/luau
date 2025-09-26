// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/DenseHash.h"
#include "Luau/EqSatSimplification.h"
#include "Luau/Set.h"
#include "Luau/SubtypingVariance.h"
#include "Luau/TypeCheckLimits.h"
#include "Luau/TypeFunction.h"
#include "Luau/TypeFwd.h"
#include "Luau/TypeIds.h"
#include "Luau/TypePairHash.h"
#include "Luau/TypePath.h"

#include <vector>
#include <optional>

namespace Luau
{

template<typename A, typename B>
struct TryPair;
struct InternalErrorReporter;

class TypeIds;
class Normalizer;
struct NormalizedExternType;
struct NormalizedFunctionType;
struct NormalizedStringType;
struct NormalizedType;
struct Property;
struct Scope;
struct TableIndexer;
struct TypeArena;
struct TypeCheckLimits;

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
inline const SubtypingReasoning kEmptyReasoning = SubtypingReasoning{TypePath::kEmpty, TypePath::kEmpty, SubtypingVariance::Invalid};

/*
 * When we encounter a generic pack over the course of a subtyping test, we need
 * to tentatively map that generic pack onto a type pack on the other side. This endeavor is complicated by the facts that the scope of generic packs
 * isn't strictly lexical (see test nested_generic_argument_type_packs), and that nested generic packs can shadow existing ones
 * such as with the type <A...>(<A...>(A...) -> (), <A...>(A...) -> ()) -> (), which should result in three independent bindings for A... .
 * To handle this, we maintain a stack of frames, each of which contains a mapping for the generic packs bound in that scope, as well as a pointers to
 * its parent and child scopes. Inside each frame, we map the generic pack to an optional type pack, which is nullopt if we have not yet encountered a
 * mapping for that generic pack in this scope.
 */

struct MappedGenericEnvironment
{
    struct MappedGenericFrame
    {
        DenseHashMap<TypePackId, std::optional<TypePackId>> mappings;
        std::optional<size_t> parentScopeIndex; // nullopt if this is the root frame
        DenseHashSet<size_t> children{0};

        MappedGenericFrame(DenseHashMap<TypePackId, std::optional<TypePackId>> mappings, std::optional<size_t> parentScopeIndex);
    };

    std::vector<MappedGenericFrame> frames;
    std::optional<size_t> currentScopeIndex = std::nullopt; // nullopt if we are in the global scope

    struct Unmapped
    {
        // The index of the scope where the generic pack was quantified
        size_t scopeIndex;
    };

    struct NotBindable
    {
    };

    using LookupResult = Luau::Variant<TypePackId, Unmapped, NotBindable>;

    // Looks up the given generic pack starting from the innermost scope and working outwards.
    // Returns Unmapped if the pack is not mapped in the current scope, and NotBindable if it is not bindable in the current or any enclosing scopes.
    LookupResult lookupGenericPack(TypePackId genericTp) const;

    // Pushes a new scope onto the stack of frames. The new scope will contain the generic packs which are being quantified bound to nullopt.
    // Also updates currentScopeIndex to point to the new frame.
    void pushFrame(const std::vector<TypePackId>& genericTps);

    // Restores the current scope to the parent of the current frame. Doesn't actually discard any mappings, since we may need them later.
    void popFrame();

    bool bindGeneric(TypePackId genericTp, TypePackId bindeeTp);
};

struct SubtypingResult
{
    bool isSubtype = false;
    bool normalizationTooComplex = false;
    bool isCacheable = true;
    ErrorVec errors;
    /// The reason for isSubtype to be false. May not be present even if
    /// isSubtype is false, depending on the input types.
    SubtypingReasonings reasoning{kEmptyReasoning};
    DenseHashMap<TypePackId, TypePackId> mappedGenericPacks_DEPRECATED{nullptr};

    // If this subtype result required testing free types, we might be making
    // assumptions about what the free type eventually resolves to.  If so,
    // those assumptions are recorded here.
    std::vector<SubtypeConstraint> assumedConstraints;

    /// If any generic bounds were invalid, report them here
    std::vector<GenericBoundsMismatch> genericBoundsMismatches;

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
        TypeIds lowerBound;
        TypeIds upperBound;
    };

    // TODO: Clip with LuauSubtypingGenericsDoesntUseVariance
    struct GenericBounds_DEPRECATED
    {
        DenseHashSet<TypeId> lowerBound{nullptr};
        DenseHashSet<TypeId> upperBound{nullptr};
    };

    /* For nested subtyping relationship tests of mapped generic bounds, we keep the outer environment immutable */
    SubtypingEnvironment* parent = nullptr;

    /// Applies `mappedGenerics` to the given type.
    /// This is used specifically to substitute for generics in type function instances.
    std::optional<TypeId> applyMappedGenerics(
        NotNull<BuiltinTypes> builtinTypes,
        NotNull<TypeArena> arena,
        TypeId ty,
        NotNull<InternalErrorReporter> iceReporter
    );
    // TODO: Clip with LuauSubtypingGenericsDoesntUseVariance
    std::optional<TypeId> applyMappedGenerics_DEPRECATED(NotNull<BuiltinTypes> builtinTypes, NotNull<TypeArena> arena, TypeId ty);

    const TypeId* tryFindSubstitution(TypeId ty) const;
    // TODO: Clip with LuauSubtypingGenericsDoesntUseVariance
    const SubtypingResult* tryFindSubtypingResult(std::pair<TypeId, TypeId> subAndSuper) const;

    bool containsMappedType(TypeId ty) const;
    bool containsMappedPack(TypePackId tp) const;

    GenericBounds& getMappedTypeBounds(TypeId ty, NotNull<InternalErrorReporter> iceReporter);
    // TODO: Clip with LuauSubtypingGenericsDoesntUseVariance
    GenericBounds_DEPRECATED& getMappedTypeBounds_DEPRECATED(TypeId ty);
    // TODO: Clip with LuauSubtypingGenericPacksDoesntUseVariance
    TypePackId* getMappedPackBounds_DEPRECATED(TypePackId tp);

    /*
     * When we encounter a generic over the course of a subtyping test, we need
     * to tentatively map that generic onto a type on the other side. We map to a
     * vector of bounds, since generics may be shadowed by nested types. The back
     * of each vector represents the current scope.
     */
    DenseHashMap<TypeId, std::vector<GenericBounds>> mappedGenerics{nullptr};
    // TODO: Clip with LuauSubtypingGenericsDoesntUseVariance
    DenseHashMap<TypeId, GenericBounds_DEPRECATED> mappedGenerics_DEPRECATED{nullptr};

    MappedGenericEnvironment mappedGenericPacks;
    // TODO: Clip with LuauSubtypingGenericPacksDoesntUseVariance
    DenseHashMap<TypePackId, TypePackId> mappedGenericPacks_DEPRECATED{nullptr};

    /*
     * See the test cyclic_tables_are_assumed_to_be_compatible_with_extern_types for
     * details.
     *
     * An empty value is equivalent to a nonexistent key.
     */
    DenseHashMap<TypeId, TypeId> substitutions{nullptr};

    // TODO: Clip with LuauSubtypingGenericsDoesntUseVariance
    DenseHashMap<std::pair<TypeId, TypeId>, SubtypingResult, TypePairHash> ephemeralCache{{}};

    // We use this cache to track pairs of subtypes that we tried to subtype, and found them to be in the seen set at the time.
    // In those situations, we return True, but mark the result as not cacheable, because we don't want to cache broader results which
    // led to the seen pair. However, those results were previously being cache in the ephemeralCache, and we still want to cache them somewhere
    // for performance reasons.
    DenseHashMap<std::pair<TypeId, TypeId>, SubtypingResult, TypePairHash> seenSetCache{{}};
};

struct Subtyping
{
    NotNull<BuiltinTypes> builtinTypes;
    NotNull<TypeArena> arena;
    NotNull<Simplifier> simplifier;
    NotNull<Normalizer> normalizer;
    NotNull<TypeFunctionRuntime> typeFunctionRuntime;
    NotNull<InternalErrorReporter> iceReporter;

    TypeCheckLimits limits;

    // If a type is known to have a single unique reference, then we can perform
    // a covariant test where an invariant test would otherwise be required.
    const DenseHashSet<TypeId>* uniqueTypes = nullptr;

    enum class Variance
    {
        Covariant,
        Contravariant
    };

    // TODO: Clip this along with LuauSubtypingGenericsDoesntUseVariance?
    Variance variance = Variance::Covariant;

    using SeenSet = Set<std::pair<TypeId, TypeId>, TypePairHash>;
    using SeenTypePackSet = Set<std::pair<TypePackId, TypePackId>, TypePairHash>;

    SeenSet seenTypes{{}};
    SeenTypePackSet seenPacks{{}};

    Subtyping(
        NotNull<BuiltinTypes> builtinTypes,
        NotNull<TypeArena> typeArena,
        NotNull<Simplifier> simplifier,
        NotNull<Normalizer> normalizer,
        NotNull<TypeFunctionRuntime> typeFunctionRuntime,
        NotNull<InternalErrorReporter> iceReporter
    );

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

    SubtypingResult isSubtype(TypeId subTy, TypeId superTy, NotNull<Scope> scope);
    SubtypingResult isSubtype(TypePackId subTp, TypePackId superTp, NotNull<Scope> scope, const std::vector<TypeId>& bindableGenerics);
    // Clip with FFlagLuauPassBindableGenericsByReference
    SubtypingResult isSubtype_DEPRECATED(
        TypePackId subTp,
        TypePackId superTp,
        NotNull<Scope> scope,
        std::optional<std::vector<TypeId>> bindableGenerics = std::nullopt
    );

private:
    DenseHashMap<std::pair<TypeId, TypeId>, SubtypingResult, TypePairHash> resultCache{{}};

    SubtypingResult cache(SubtypingEnvironment& env, SubtypingResult res, TypeId subTy, TypeId superTy);

    SubtypingResult isCovariantWith(SubtypingEnvironment& env, TypeId subTy, TypeId superTy, NotNull<Scope> scope);

    template<typename SubTy, typename SuperTy>
    SubtypingResult isContravariantWith(SubtypingEnvironment& env, SubTy&& subTy, SuperTy&& superTy, NotNull<Scope> scope);

    template<typename SubTy, typename SuperTy>
    SubtypingResult isInvariantWith(SubtypingEnvironment& env, SubTy&& subTy, SuperTy&& superTy, NotNull<Scope> scope);

    template<typename SubTy, typename SuperTy>
    SubtypingResult isCovariantWith(SubtypingEnvironment& env, const TryPair<const SubTy*, const SuperTy*>& pair, NotNull<Scope> scope);

    template<typename SubTy, typename SuperTy>
    SubtypingResult isContravariantWith(SubtypingEnvironment& env, const TryPair<const SubTy*, const SuperTy*>& pair, NotNull<Scope>);

    template<typename SubTy, typename SuperTy>
    SubtypingResult isInvariantWith(SubtypingEnvironment& env, const TryPair<const SubTy*, const SuperTy*>& pair, NotNull<Scope>);

    SubtypingResult isCovariantWith(SubtypingEnvironment& env, TypeId subTy, const UnionType* superUnion, NotNull<Scope> scope);
    SubtypingResult isCovariantWith(SubtypingEnvironment& env, const UnionType* subUnion, TypeId superTy, NotNull<Scope> scope);
    SubtypingResult isCovariantWith(SubtypingEnvironment& env, TypeId subTy, const IntersectionType* superIntersection, NotNull<Scope> scope);
    SubtypingResult isCovariantWith(SubtypingEnvironment& env, const IntersectionType* subIntersection, TypeId superTy, NotNull<Scope> scope);

    SubtypingResult isCovariantWith(SubtypingEnvironment& env, const NegationType* subNegation, TypeId superTy, NotNull<Scope> scope);
    SubtypingResult isCovariantWith(SubtypingEnvironment& env, const TypeId subTy, const NegationType* superNegation, NotNull<Scope> scope);

    SubtypingResult isCovariantWith(SubtypingEnvironment& env, const PrimitiveType* subPrim, const PrimitiveType* superPrim, NotNull<Scope> scope);
    SubtypingResult isCovariantWith(
        SubtypingEnvironment& env,
        const SingletonType* subSingleton,
        const PrimitiveType* superPrim,
        NotNull<Scope> scope
    );
    SubtypingResult isCovariantWith(
        SubtypingEnvironment& env,
        const SingletonType* subSingleton,
        const SingletonType* superSingleton,
        NotNull<Scope> scope
    );
    SubtypingResult isCovariantWith(SubtypingEnvironment& env, const TableType* subTable, const TableType* superTable, NotNull<Scope> scope);
    SubtypingResult isCovariantWith(
        SubtypingEnvironment& env,
        const TableType* subTable,
        const TableType* superTable,
        bool forceCovariantTest,
        NotNull<Scope> scope
    );
    SubtypingResult isCovariantWith(SubtypingEnvironment& env, const MetatableType* subMt, const MetatableType* superMt, NotNull<Scope> scope);
    SubtypingResult isCovariantWith(SubtypingEnvironment& env, const MetatableType* subMt, const TableType* superTable, NotNull<Scope> scope);
    SubtypingResult isCovariantWith(
        SubtypingEnvironment& env,
        const ExternType* subExternType,
        const ExternType* superExternType,
        NotNull<Scope> scope
    );
    SubtypingResult isCovariantWith(
        SubtypingEnvironment& env,
        TypeId subTy,
        const ExternType* subExternType,
        TypeId superTy,
        const TableType* superTable,
        NotNull<Scope>
    );
    SubtypingResult isCovariantWith(
        SubtypingEnvironment& env,
        const FunctionType* subFunction,
        const FunctionType* superFunction,
        NotNull<Scope> scope
    );
    SubtypingResult isCovariantWith(SubtypingEnvironment& env, const TableType* subTable, const PrimitiveType* superPrim, NotNull<Scope> scope);
    SubtypingResult isCovariantWith(SubtypingEnvironment& env, const PrimitiveType* subPrim, const TableType* superTable, NotNull<Scope> scope);
    SubtypingResult isCovariantWith(SubtypingEnvironment& env, const SingletonType* subSingleton, const TableType* superTable, NotNull<Scope> scope);

    SubtypingResult isCovariantWith(
        SubtypingEnvironment& env,
        const TableIndexer& subIndexer,
        const TableIndexer& superIndexer,
        NotNull<Scope> scope
    );
    SubtypingResult isCovariantWith(
        SubtypingEnvironment& env,
        const Property& subProperty,
        const Property& superProperty,
        const std::string& name,
        bool forceCovariantTest,
        NotNull<Scope> scope
    );

    SubtypingResult isCovariantWith(
        SubtypingEnvironment& env,
        const std::shared_ptr<const NormalizedType>& subNorm,
        const std::shared_ptr<const NormalizedType>& superNorm,
        NotNull<Scope> scope
    );
    SubtypingResult isCovariantWith(
        SubtypingEnvironment& env,
        const NormalizedExternType& subExternType,
        const NormalizedExternType& superExternType,
        NotNull<Scope> scope
    );
    SubtypingResult isCovariantWith(
        SubtypingEnvironment& env,
        const NormalizedExternType& subExternType,
        const TypeIds& superTables,
        NotNull<Scope> scope
    );
    SubtypingResult isCovariantWith(
        SubtypingEnvironment& env,
        const NormalizedStringType& subString,
        const NormalizedStringType& superString,
        NotNull<Scope> scope
    );
    SubtypingResult isCovariantWith(
        SubtypingEnvironment& env,
        const NormalizedStringType& subString,
        const TypeIds& superTables,
        NotNull<Scope> scope
    );
    SubtypingResult isCovariantWith(
        SubtypingEnvironment& env,
        const NormalizedFunctionType& subFunction,
        const NormalizedFunctionType& superFunction,
        NotNull<Scope>
    );
    SubtypingResult isCovariantWith(SubtypingEnvironment& env, const TypeIds& subTypes, const TypeIds& superTypes, NotNull<Scope> scope);

    SubtypingResult isCovariantWith(
        SubtypingEnvironment& env,
        const VariadicTypePack* subVariadic,
        const VariadicTypePack* superVariadic,
        NotNull<Scope> scope
    );
    SubtypingResult isCovariantWith(
        SubtypingEnvironment& env,
        const TypeFunctionInstanceType* subFunctionInstance,
        const TypeId superTy,
        NotNull<Scope> scope
    );
    SubtypingResult isCovariantWith(
        SubtypingEnvironment& env,
        const TypeId subTy,
        const TypeFunctionInstanceType* superFunctionInstance,
        NotNull<Scope> scope
    );

    // Pack subtyping
    SubtypingResult isCovariantWith(SubtypingEnvironment& env, TypePackId subTp, TypePackId superTp, NotNull<Scope> scope);
    std::optional<SubtypingResult> isSubTailCovariantWith(
        SubtypingEnvironment& env,
        std::vector<SubtypingResult>& outputResults,
        TypePackId subTp,
        TypePackId subTail,
        TypePackId superTp,
        size_t superHeadStartIndex,
        const std::vector<TypeId>& superHead,
        std::optional<TypePackId> superTail,
        NotNull<Scope> scope
    );
    std::optional<SubtypingResult> isCovariantWithSuperTail(
        SubtypingEnvironment& env,
        std::vector<SubtypingResult>& outputResults,
        TypePackId subTp,
        size_t subHeadStartIndex,
        const std::vector<TypeId>& subHead,
        std::optional<TypePackId> subTail,
        TypePackId superTp,
        TypePackId superTail,
        NotNull<Scope> scope
    );

    bool bindGeneric(SubtypingEnvironment& env, TypeId subTp, TypeId superTp);
    // Clip with LuauSubtypingGenericPacksDoesntUseVariance
    bool bindGeneric_DEPRECATED(SubtypingEnvironment& env, TypePackId subTp, TypePackId superTp) const;

    template<typename T, typename Container>
    TypeId makeAggregateType(const Container& container, TypeId orElse);

    std::pair<TypeId, ErrorVec> handleTypeFunctionReductionResult(const TypeFunctionInstanceType* functionInstance, NotNull<Scope> scope);

    [[noreturn]] void unexpected(TypeId ty);
    [[noreturn]] void unexpected(TypePackId tp);

    SubtypingResult trySemanticSubtyping(SubtypingEnvironment& env, TypeId subTy, TypeId superTy, NotNull<Scope> scope, SubtypingResult& original);

    SubtypingResult checkGenericBounds(
        const SubtypingEnvironment::GenericBounds& bounds,
        SubtypingEnvironment& env,
        NotNull<Scope> scope,
        std::string_view genericName
    );

    // TODO: Clip with LuauSubtypingReportGenericBoundMismatches
    SubtypingResult checkGenericBounds_DEPRECATED(const SubtypingEnvironment::GenericBounds& bounds, SubtypingEnvironment& env, NotNull<Scope> scope);

    static void maybeUpdateBounds(
        TypeId here,
        TypeId there,
        TypeIds& boundsToUpdate,
        const TypeIds& firstBoundsToCheck,
        const TypeIds& secondBoundsToCheck
    );
};

} // namespace Luau
