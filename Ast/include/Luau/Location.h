// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include <string>
namespace Luau
{

struct Position
{
    unsigned int line, column;

    Position(unsigned int line, unsigned int column);

    bool operator==(const Position& rhs) const;
    bool operator!=(const Position& rhs) const;
    bool operator<(const Position& rhs) const;
    bool operator>(const Position& rhs) const;
    bool operator<=(const Position& rhs) const;
    bool operator>=(const Position& rhs) const;

    void shift(const Position& start, const Position& oldEnd, const Position& newEnd);
};

struct Location
{
    Position begin, end;

    Location();
    Location(const Position& begin, const Position& end);
    Location(const Position& begin, unsigned int length);
    Location(const Location& begin, const Location& end);

    bool operator==(const Location& rhs) const;
    bool operator!=(const Location& rhs) const;

    bool encloses(const Location& l) const;
    bool overlaps(const Location& l) const;
    bool contains(const Position& p) const;
    bool containsClosed(const Position& p) const;
    void extend(const Location& other);
    void shift(const Position& start, const Position& oldEnd, const Position& newEnd);
};

} // namespace Luau
