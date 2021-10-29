// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Fixture.h"

#include "Luau/Ast.h"

#include "doctest.h"

using namespace Luau;

namespace
{

class AstVisitorTracking : public AstVisitor
{
private:
    std::vector<AstNode*> visitedNodes;
    std::set<size_t> seen;

public:
    bool visit(AstNode* n) override
    {
        visitedNodes.push_back(n);
        return true;
    }

    AstNode* operator[](size_t index)
    {
        REQUIRE(index < visitedNodes.size());

        seen.insert(index);
        return visitedNodes[index];
    }

    ~AstVisitorTracking()
    {
        std::string s = "Seen " + std::to_string(seen.size()) + " nodes but got " + std::to_string(visitedNodes.size());
        CHECK_MESSAGE(seen.size() == visitedNodes.size(), s);
    }
};

class AstTypeVisitorTrackingWiths : public AstVisitorTracking
{
public:
    using AstVisitorTracking::visit;
    bool visit(AstType* n) override
    {
        return visit((AstNode*)n);
    }
};

} // namespace

TEST_SUITE_BEGIN("AstVisitorTest");

TEST_CASE_FIXTURE(Fixture, "TypeAnnotationsAreNotVisited")
{
    AstStatBlock* block = parse(R"(
        local a: A<number>
    )");

    AstVisitorTracking v;
    block->visit(&v);

    CHECK(v[0]->is<AstStatBlock>());
    CHECK(v[1]->is<AstStatLocal>());
    // We should not have v[2] that points to the annotation
    // We should not have v[3] that points to the type argument 'number' in A.
}

TEST_CASE_FIXTURE(Fixture, "LocalTwoBindings")
{
    AstStatBlock* block = parse(R"(
        local a, b
    )");

    AstVisitorTracking v;
    block->visit(&v);

    CHECK(v[0]->is<AstStatBlock>());
    CHECK(v[1]->is<AstStatLocal>());
}

TEST_CASE_FIXTURE(Fixture, "LocalTwoAnnotatedBindings")
{
    AstStatBlock* block = parse(R"(
        local a: A, b: B<number>
    )");

    AstTypeVisitorTrackingWiths v;
    block->visit(&v);

    CHECK(v[0]->is<AstStatBlock>());
    CHECK(v[1]->is<AstStatLocal>());
    CHECK(v[2]->is<AstTypeReference>());
    CHECK(v[3]->is<AstTypeReference>());
    CHECK(v[4]->is<AstTypeReference>());
}

TEST_CASE_FIXTURE(Fixture, "LocalTwoAnnotatedBindingsWithTwoValues")
{
    AstStatBlock* block = parse(R"(
        local a: A, b: B<number> = 1, 2
    )");

    AstTypeVisitorTrackingWiths v;
    block->visit(&v);

    CHECK(v[0]->is<AstStatBlock>());
    CHECK(v[1]->is<AstStatLocal>());
    CHECK(v[2]->is<AstTypeReference>());
    CHECK(v[3]->is<AstTypeReference>());
    CHECK(v[4]->is<AstTypeReference>());
    CHECK(v[5]->is<AstExprConstantNumber>());
    CHECK(v[6]->is<AstExprConstantNumber>());
}

TEST_SUITE_END();
