// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/TopoSortStatements.h"

/* Decide the order in which we typecheck Lua statements in a block.
 *
 * Algorithm:
 *
 *     1. Build up a dependency graph.
 *          i.   An AstStat is said to depend on another AstStat if it refers to it in any child node.
 *               A dependency is the relationship between the declaration of a symbol and its uses.
 *          ii.  Additionally, statements that do not define functions have a dependency on the previous non-function statement.  We do this
 *               to prevent the algorithm from checking imperative statements out-of-order.
 *     2. Walk each node in the graph in lexical order.  For each node:
 *          i.   Select the next thing `t`
 *          ii.  If `t` has no dependencies at all and is not a function definition, check it now
 *          iii. If `t` is a function definition or an expression that does not include a function call, add it to a queue `Q`.
 *          iv.  Else, toposort `Q` and check things until it is possible to check `t`
 *              * If this fails, we expect the Lua runtime to also fail, as the code is trying to use a symbol before it has been defined.
 *     3. Toposort whatever remains in `Q` and check it all.
 *
 * The end result that we want satisfies a few qualities:
 *
 *     1. Things are generally checked in lexical order.
 *     2. If a function F calls another function G that is declared out-of-order, but in a way that will work when the code is actually run, we want
 * to check G before F.
 *     3. Cyclic dependencies can be resolved by picking an arbitrary statement to check first.
 */

#include "Luau/Parser.h"
#include "Luau/DenseHash.h"
#include "Luau/Common.h"

#include <algorithm>
#include <deque>
#include <list>
#include <map>
#include <memory>
#include <set>
#include <stdexcept>
#include <optional>

namespace Luau
{

// For some reason, natvis interacts really poorly with anonymous data types
namespace detail
{

struct Identifier
{
    std::string name;    // A nice textual name
    const AstLocal* ctx; // Only used to disambiguate potentially shadowed names
};

bool operator==(const Identifier& lhs, const Identifier& rhs)
{
    return lhs.name == rhs.name && lhs.ctx == rhs.ctx;
}

struct IdentifierHash
{
    size_t operator()(const Identifier& ident) const
    {
        return std::hash<std::string>()(ident.name) ^ std::hash<const void*>()(ident.ctx);
    }
};

struct Node;

struct Arcs
{
    std::set<Node*> provides;
    std::set<Node*> depends;
};

struct Node : Arcs
{
    std::optional<Identifier> name;
    AstStat* element;

    Node(const std::optional<Identifier>& name, AstStat* el)
        : name(name)
        , element(el)
    {
    }
};

using NodeQueue = std::deque<std::unique_ptr<Node>>;
using NodeList = std::list<std::unique_ptr<Node>>;

std::optional<Identifier> mkName(const AstExpr& expr);

Identifier mkName(const AstLocal& local)
{
    return {local.name.value, &local};
}

Identifier mkName(const AstExprLocal& local)
{
    return mkName(*local.local);
}

Identifier mkName(const AstExprGlobal& global)
{
    return {global.name.value, nullptr};
}

Identifier mkName(const AstName& name)
{
    return {name.value, nullptr};
}

std::optional<Identifier> mkName(const AstExprIndexName& expr)
{
    auto lhs = mkName(*expr.expr);
    if (lhs)
    {
        std::string s = std::move(lhs->name);
        s += ".";
        s += expr.index.value;
        return Identifier{std::move(s), lhs->ctx};
    }
    else
        return std::nullopt;
}

Identifier mkName(const AstExprError& expr)
{
    return {format("error#%d", expr.messageIndex), nullptr};
}

std::optional<Identifier> mkName(const AstExpr& expr)
{
    if (auto l = expr.as<AstExprLocal>())
        return mkName(*l);
    else if (auto g = expr.as<AstExprGlobal>())
        return mkName(*g);
    else if (auto i = expr.as<AstExprIndexName>())
        return mkName(*i);
    else if (auto e = expr.as<AstExprError>())
        return mkName(*e);

    return std::nullopt;
}

Identifier mkName(const AstStatFunction& function)
{
    auto name = mkName(*function.name);
    LUAU_ASSERT(bool(name));
    if (!name)
        throw std::runtime_error("Internal error: Function declaration has a bad name");

    return *name;
}

Identifier mkName(const AstStatLocalFunction& function)
{
    return mkName(*function.name);
}

std::optional<Identifier> mkName(const AstStatAssign& assign)
{
    if (assign.vars.size != 1)
        return std::nullopt;

    return mkName(*assign.vars.data[0]);
}

std::optional<Identifier> mkName(const AstStatLocal& local)
{
    if (local.vars.size != 1)
        return std::nullopt;

    return mkName(*local.vars.data[0]);
}

Identifier mkName(const AstStatTypeAlias& typealias)
{
    return mkName(typealias.name);
}

std::optional<Identifier> mkName(AstStat* const el)
{
    if (auto function = el->as<AstStatFunction>())
        return mkName(*function);
    else if (auto function = el->as<AstStatLocalFunction>())
        return mkName(*function);
    else if (auto assign = el->as<AstStatAssign>())
        return mkName(*assign);
    else if (auto local = el->as<AstStatLocal>())
        return mkName(*local);
    else if (auto typealias = el->as<AstStatTypeAlias>())
        return mkName(*typealias);

    return std::nullopt;
}

struct ArcCollector : public AstVisitor
{
    NodeQueue& queue;
    DenseHashMap<Identifier, Node*, IdentifierHash> map;

    Node* currentArc;

    ArcCollector(NodeQueue& queue)
        : queue(queue)
        , map(Identifier{std::string{}, 0})
        , currentArc(nullptr)
    {
        for (const auto& node : queue)
        {
            if (node->name && !map.contains(*node->name))
                map[*node->name] = node.get();
        }
    }

    void add(const Identifier& name)
    {
        Node** it = map.find(name);
        if (it == nullptr)
            return;

        Node* n = *it;

        if (n == currentArc)
            return;

        n->provides.insert(currentArc);
        currentArc->depends.insert(n);
    }

    bool visit(AstExprGlobal* node) override
    {
        add(mkName(*node));
        return true;
    }

    bool visit(AstExprLocal* node) override
    {
        add(mkName(*node));
        return true;
    }

    bool visit(AstExprIndexName* node) override
    {
        auto name = mkName(*node);
        if (name)
            add(*name);
        return true;
    }

    bool visit(AstStatFunction* node) override
    {
        auto name = mkName(*node->name);
        if (!name)
            throw std::runtime_error("Internal error: AstStatFunction has a bad name");

        add(*name);
        return true;
    }

    bool visit(AstStatLocalFunction* node) override
    {
        add(mkName(*node->name));
        return true;
    }

    bool visit(AstStatAssign* node) override
    {
        return true;
    }

    bool visit(AstStatTypeAlias* node) override
    {
        add(mkName(*node));
        return true;
    }

    bool visit(AstType* node) override
    {
        return true;
    }

    bool visit(AstTypeReference* node) override
    {
        add(mkName(node->name));
        return true;
    }

    bool visit(AstTypeTypeof* node) override
    {
        std::optional<Identifier> name = mkName(*node->expr);
        if (name)
            add(*name);
        return true;
    }
};

struct ContainsFunctionCall : public AstVisitor
{
    bool result = false;

    bool visit(AstExpr*) override
    {
        return !result; // short circuit if result is true
    }

    bool visit(AstExprCall*) override
    {
        result = true;
        return false;
    }

    bool visit(AstStatForIn*) override
    {
        // for in loops perform an implicit function call as part of the iterator protocol
        result = true;
        return false;
    }

    bool visit(AstExprFunction*) override
    {
        return false;
    }
    bool visit(AstStatFunction*) override
    {
        return false;
    }
    bool visit(AstStatLocalFunction*) override
    {
        return false;
    }

    bool visit(AstType* ta) override
    {
        return true;
    }
};

bool isToposortableNode(const AstStat& stat)
{
    return isFunction(stat) || stat.is<AstStatTypeAlias>();
}

bool containsToposortableNode(const std::vector<AstStat*>& block)
{
    for (AstStat* stat : block)
        if (isToposortableNode(*stat))
            return true;

    return false;
}

bool isBlockTerminator(const AstStat& stat)
{
    return stat.is<AstStatReturn>() || stat.is<AstStatBreak>() || stat.is<AstStatContinue>();
}

// Clip arcs to and from the node
void prune(Node* next)
{
    for (const auto& node : next->provides)
    {
        auto it = node->depends.find(next);
        LUAU_ASSERT(it != node->depends.end());
        node->depends.erase(it);
    }

    for (const auto& node : next->depends)
    {
        auto it = node->provides.find(next);
        LUAU_ASSERT(it != node->provides.end());
        node->provides.erase(it);
    }
}

// Drain Q until the target's depends arcs are satisfied.  target is always added to the result.
void drain(NodeList& Q, std::vector<AstStat*>& result, Node* target)
{
    // Trying to toposort a subgraph is a pretty big hassle. :(
    // Some of the nodes in .depends and .provides aren't present in our subgraph

    std::map<Node*, Arcs> allArcs;

    for (auto& node : Q)
    {
        // Copy the connectivity information but filter out any provides or depends arcs that are not in Q
        Arcs& arcs = allArcs[node.get()];

        DenseHashSet<Node*> elements{nullptr};
        for (const auto& q : Q)
            elements.insert(q.get());

        for (Node* node : node->depends)
        {
            if (elements.contains(node))
                arcs.depends.insert(node);
        }
        for (Node* node : node->provides)
        {
            if (elements.contains(node))
                arcs.provides.insert(node);
        }
    }

    while (!Q.empty())
    {
        if (target && target->depends.empty())
        {
            prune(target);
            result.push_back(target->element);
            return;
        }

        std::unique_ptr<Node> nextNode;

        for (auto iter = Q.begin(); iter != Q.end(); ++iter)
        {
            if (isBlockTerminator(*iter->get()->element))
                continue;

            LUAU_ASSERT(allArcs.end() != allArcs.find(iter->get()));
            const Arcs& arcs = allArcs[iter->get()];

            if (arcs.depends.empty())
            {
                nextNode = std::move(*iter);
                Q.erase(iter);
                break;
            }
        }

        if (!nextNode)
        {
            // We've hit a cycle or a terminator. Pick an arbitrary node.
            nextNode = std::move(Q.front());
            Q.pop_front();
        }

        for (const auto& node : nextNode->provides)
        {
            auto it = allArcs.find(node);
            if (allArcs.end() != it)
            {
                auto i2 = it->second.depends.find(nextNode.get());
                LUAU_ASSERT(i2 != it->second.depends.end());
                it->second.depends.erase(i2);
            }
        }

        for (const auto& node : nextNode->depends)
        {
            auto it = allArcs.find(node);
            if (allArcs.end() != it)
            {
                auto i2 = it->second.provides.find(nextNode.get());
                LUAU_ASSERT(i2 != it->second.provides.end());
                it->second.provides.erase(i2);
            }
        }

        prune(nextNode.get());
        result.push_back(nextNode->element);
    }

    if (target)
    {
        prune(target);
        result.push_back(target->element);
    }
}

} // namespace detail

bool containsFunctionCall(const AstStat& stat)
{
    detail::ContainsFunctionCall cfc;
    const_cast<AstStat&>(stat).visit(&cfc);
    return cfc.result;
}

bool isFunction(const AstStat& stat)
{
    return stat.is<AstStatFunction>() || stat.is<AstStatLocalFunction>();
}

void toposort(std::vector<AstStat*>& stats)
{
    using namespace detail;

    if (stats.empty())
        return;

    if (!containsToposortableNode(stats))
        return;

    std::vector<AstStat*> result;
    result.reserve(stats.size());

    NodeQueue nodes;
    NodeList Q;

    for (AstStat* stat : stats)
        nodes.push_back(std::unique_ptr<Node>(new Node(mkName(stat), stat)));

    ArcCollector collector{nodes};

    for (const auto& node : nodes)
    {
        collector.currentArc = node.get();
        node->element->visit(&collector);
    }

    {
        auto it = nodes.begin();
        auto prev = it;

        while (it != nodes.end())
        {
            if (it != prev && !isToposortableNode(*(*it)->element))
            {
                (*it)->depends.insert(prev->get());
                (*prev)->provides.insert(it->get());
                prev = it;
            }
            ++it;
        }
    }

    while (!nodes.empty())
    {
        Node* next = nodes.front().get();

        if (next->depends.empty() && !isBlockTerminator(*next->element))
        {
            prune(next);
            result.push_back(next->element);
        }
        else if (!containsFunctionCall(*next->element))
            Q.push_back(std::move(nodes.front()));
        else
            drain(Q, result, next);

        nodes.pop_front();
    }

    drain(Q, result, nullptr);

    std::swap(stats, result);
}

} // namespace Luau
