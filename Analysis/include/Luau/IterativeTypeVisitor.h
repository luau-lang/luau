// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/Type.h"
#include "Luau/TypePack.h"
#include "Luau/Set.h"

#include <string>

namespace Luau
{

struct IterativeTypeVisitor
{
    using SeenSet = Set<const void*>;

    // We avoid Luau::Variant here because we can move the tag bit and make this struct 64 bits shorter.
    struct WorkItem
    {
        WorkItem(TypeId ty, int32_t parent);
        WorkItem(TypePackId tp, int32_t parent);

        const TypeId* asType() const;
        const TypePackId* asTypePack() const;

        bool operator==(TypeId ty) const;
        bool operator==(TypePackId tp) const;

    private:
        // TypeId if isType, else TypePackId
        const void* t;
        bool isType;

    public:
        // -1 indicates no parent
        int32_t parent;
    };

    explicit IterativeTypeVisitor(std::string visitorName, bool skipBoundTypes);
    explicit IterativeTypeVisitor(std::string visitorName, bool visitOnce, bool skipBoundTypes);
    explicit IterativeTypeVisitor(std::string visitorName, SeenSet seen, bool visitOnce, bool skipBoundTypes);

    IterativeTypeVisitor(const IterativeTypeVisitor&) = delete;
    IterativeTypeVisitor& operator=(const IterativeTypeVisitor&) = delete;

    virtual ~IterativeTypeVisitor() = default;

    virtual void cycle(TypeId);
    virtual void cycle(TypePackId);

    virtual bool visit(TypeId ty);
    virtual bool visit(TypeId ty, const BoundType& btv);
    virtual bool visit(TypeId ty, const FreeType& ftv);
    virtual bool visit(TypeId ty, const GenericType& gtv);
    virtual bool visit(TypeId ty, const ErrorType& etv);
    virtual bool visit(TypeId ty, const PrimitiveType& ptv);
    virtual bool visit(TypeId ty, const FunctionType& ftv);
    virtual bool visit(TypeId ty, const TableType& ttv);
    virtual bool visit(TypeId ty, const MetatableType& mtv);
    virtual bool visit(TypeId ty, const ExternType& etv);
    virtual bool visit(TypeId ty, const AnyType& atv);
    virtual bool visit(TypeId ty, const NoRefineType& nrt);
    virtual bool visit(TypeId ty, const UnknownType& utv);
    virtual bool visit(TypeId ty, const NeverType& ntv);
    virtual bool visit(TypeId ty, const UnionType& utv);
    virtual bool visit(TypeId ty, const IntersectionType& itv);
    virtual bool visit(TypeId ty, const BlockedType& btv);
    virtual bool visit(TypeId ty, const PendingExpansionType& petv);
    virtual bool visit(TypeId ty, const SingletonType& stv);
    virtual bool visit(TypeId ty, const NegationType& ntv);
    virtual bool visit(TypeId ty, const TypeFunctionInstanceType& tfit);

    virtual bool visit(TypePackId tp);
    virtual bool visit(TypePackId tp, const BoundTypePack& btp);
    virtual bool visit(TypePackId tp, const FreeTypePack& ftp);
    virtual bool visit(TypePackId tp, const GenericTypePack& gtp);
    virtual bool visit(TypePackId tp, const ErrorTypePack& etp);
    virtual bool visit(TypePackId tp, const TypePack& pack);
    virtual bool visit(TypePackId tp, const VariadicTypePack& vtp);
    virtual bool visit(TypePackId tp, const BlockedTypePack& btp);
    virtual bool visit(TypePackId tp, const TypeFunctionInstanceTypePack& tfitp);

    void run(TypeId ty);
    void run(TypePackId tp);

protected:
    /// Add this type (or pack) to the queue of things to traverse. Does not
    /// immediately process the thing!  You cannot use this to effect in-order
    /// traversal.
    void traverse(TypeId ty);
    void traverse(TypePackId tp);

private:
    void process(TypeId ty);
    void process(TypePackId tp);

    bool hasSeen(const void* tv);
    void unsee(const void* tv);

    template<typename TID>
    bool isCyclic(TID ty) const;

    void processWorkQueue();

    SeenSet seen{nullptr};

    std::vector<WorkItem> workQueue;
    int32_t parentCursor = -1;
    uint32_t workCursor = 0;

    const std::string visitorName;
    bool skipBoundTypes = false;
    bool visitOnce = true;
};

} // namespace Luau
