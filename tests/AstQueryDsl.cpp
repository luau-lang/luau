// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "AstQueryDsl.h"

namespace Luau
{

FindNthOccurenceOf::FindNthOccurenceOf(Nth nth)
    : requestedNth(nth)
{
}

bool FindNthOccurenceOf::checkIt(AstNode* n)
{
    if (theNode)
        return false;

    if (n->classIndex == requestedNth.classIndex)
    {
        // Human factor: the requestedNth starts from 1 because of the term `nth`.
        if (currentOccurrence + 1 != requestedNth.nth)
            ++currentOccurrence;
        else
            theNode = n;
    }

    return !theNode; // once found, returns false and stops traversal
}

bool FindNthOccurenceOf::visit(AstNode* n)
{
    return checkIt(n);
}

bool FindNthOccurenceOf::visit(AstType* t)
{
    return checkIt(t);
}

bool FindNthOccurenceOf::visit(AstTypePack* t)
{
    return checkIt(t);
}

} // namespace Luau
