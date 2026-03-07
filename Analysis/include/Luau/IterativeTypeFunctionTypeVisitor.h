// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/TypeFunctionRuntime.h"
#include "Luau/TypeFwd.h"
#include "Luau/Set.h"

namespace Luau
{

struct IterativeTypeFunctionTypeVisitor
{
    using SeenSet = Set<const void*>;

    // We avoid Luau::Variant here because we can move the tag bit and make this struct 64 bits shorter.
    struct WorkItem
    {
        WorkItem(TypeFunctionTypeId ty, int32_t parent);
        WorkItem(TypeFunctionTypePackId tp, int32_t parent);

        const TypeFunctionTypeId* asType() const;
        const TypeFunctionTypePackId* asTypePack() const;

        bool operator==(TypeFunctionTypeId ty) const;
        bool operator==(TypeFunctionTypePackId tp) const;

    private:
        // TypeFunctionTypeId if isType, else TypeFunctionTypePackId
        const void* t;
        bool isType;

    public:
        // -1 indicates no parent
        int32_t parent;
    };

    explicit IterativeTypeFunctionTypeVisitor(std::string visitorName);
    explicit IterativeTypeFunctionTypeVisitor(std::string visitorName, bool visitOnce);
    explicit IterativeTypeFunctionTypeVisitor(std::string visitorName, SeenSet seen, bool visitOnce);

    IterativeTypeFunctionTypeVisitor(const IterativeTypeFunctionTypeVisitor&) = delete;
    IterativeTypeFunctionTypeVisitor& operator=(const IterativeTypeFunctionTypeVisitor&) = delete;

    virtual ~IterativeTypeFunctionTypeVisitor() = default;

    virtual void cycle(TypeFunctionTypeId ty);
    virtual void cycle(TypeFunctionTypePackId tp);

    virtual bool visit(TypeFunctionTypeId ty);
    virtual bool visit(TypeFunctionTypeId ty, const TypeFunctionPrimitiveType& tfpt);
    virtual bool visit(TypeFunctionTypeId ty, const TypeFunctionAnyType& tfat);
    virtual bool visit(TypeFunctionTypeId ty, const TypeFunctionUnknownType& tfut);
    virtual bool visit(TypeFunctionTypeId ty, const TypeFunctionNeverType& tfnt);
    virtual bool visit(TypeFunctionTypeId ty, const TypeFunctionSingletonType& tfst);
    virtual bool visit(TypeFunctionTypeId ty, const TypeFunctionUnionType& tfut);
    virtual bool visit(TypeFunctionTypeId ty, const TypeFunctionIntersectionType& tfit);
    virtual bool visit(TypeFunctionTypeId ty, const TypeFunctionNegationType& tfnt);
    virtual bool visit(TypeFunctionTypeId ty, const TypeFunctionFunctionType& tfft);
    virtual bool visit(TypeFunctionTypeId ty, const TypeFunctionTableType& tftt);
    virtual bool visit(TypeFunctionTypeId ty, const TypeFunctionExternType& tfet);
    virtual bool visit(TypeFunctionTypeId ty, const TypeFunctionGenericType& tfgt);

    virtual bool visit(TypeFunctionTypePackId tp);
    virtual bool visit(TypeFunctionTypePackId tp, const TypeFunctionTypePack& tftp);
    virtual bool visit(TypeFunctionTypePackId tp, const TypeFunctionVariadicTypePack& tfvtp);
    virtual bool visit(TypeFunctionTypePackId tp, const TypeFunctionGenericTypePack& tfgtp);

    void run(TypeFunctionTypeId ty);
    void run(TypeFunctionTypePackId tp);

protected:
    /// Add this type (or pack) to the queue of things to traverse. Does not
    /// immediately process the thing!  You cannot use this to effect in-order
    /// traversal.
    void traverse(TypeFunctionTypeId ty);
    void traverse(TypeFunctionTypePackId tp);

private:
    void process(TypeFunctionTypeId ty);
    void process(TypeFunctionTypePackId tp);

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
    bool visitOnce = true;
};

} // namespace Luau
