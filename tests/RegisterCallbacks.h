// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include <unordered_set>
#include <string>

namespace Luau
{

using RegisterCallback = void (*)();

/// Gets a set of callbacks to run immediately before running tests, intended
/// for registering new tests at runtime.
std::unordered_set<RegisterCallback>& getRegisterCallbacks();

/// Adds a new callback to be ran immediately before running tests.
///
/// @param cb the callback to add.
/// @returns a dummy integer to satisfy a doctest internal contract.
int addTestCallback(RegisterCallback cb);

} // namespace Luau
