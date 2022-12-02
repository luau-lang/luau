// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include <string>

namespace Luau
{

struct Position
{
    unsigned int line, column;

    Position(unsigned int line, unsigned int column)
        : line(line)
        , column(column)
    {
    }

    bool operator==(const Position& rhs) const
    {
        return this->column == rhs.column && this->line == rhs.line;
    }
    bool operator!=(const Position& rhs) const
    {
        return !(*this == rhs);
    }

    bool operator<(const Position& rhs) const
    {
        if (line == rhs.line)
            return column < rhs.column;
        else
            return line < rhs.line;
    }

    bool operator>(const Position& rhs) const
    {
        if (line == rhs.line)
            return column > rhs.column;
        else
            return line > rhs.line;
    }

    bool operator<=(const Position& rhs) const
    {
        return *this == rhs || *this < rhs;
    }

    bool operator>=(const Position& rhs) const
    {
        return *this == rhs || *this > rhs;
    }

    void shift(const Position& start, const Position& oldEnd, const Position& newEnd)
    {
        if (*this >= start)
        {
            if (this->line > oldEnd.line)
                this->line += (newEnd.line - oldEnd.line);
            else
            {
                this->line = newEnd.line;
                this->column += (newEnd.column - oldEnd.column);
            }
        }
    }
};

struct Location
{
    Position begin, end;

    Location()
        : begin(0, 0)
        , end(0, 0)
    {
    }

    Location(const Position& begin, const Position& end)
        : begin(begin)
        , end(end)
    {
    }

    Location(const Position& begin, unsigned int length)
        : begin(begin)
        , end(begin.line, begin.column + length)
    {
    }

    Location(const Location& begin, const Location& end)
        : begin(begin.begin)
        , end(end.end)
    {
    }

    bool operator==(const Location& rhs) const
    {
        return this->begin == rhs.begin && this->end == rhs.end;
    }
    bool operator!=(const Location& rhs) const
    {
        return !(*this == rhs);
    }

    bool encloses(const Location& l) const
    {
        return begin <= l.begin && end >= l.end;
    }
    bool overlaps(const Location& l) const
    {
        return (begin <= l.begin && end >= l.begin) || (begin <= l.end && end >= l.end) || (begin >= l.begin && end <= l.end);
    }
    bool contains(const Position& p) const
    {
        return begin <= p && p < end;
    }
    bool containsClosed(const Position& p) const
    {
        return begin <= p && p <= end;
    }
    void extend(const Location& other)
    {
        if (other.begin < begin)
            begin = other.begin;
        if (other.end > end)
            end = other.end;
    }
    void shift(const Position& start, const Position& oldEnd, const Position& newEnd)
    {
        begin.shift(start, oldEnd, newEnd);
        end.shift(start, oldEnd, newEnd);
    }
};

std::string toString(const Position& position);
std::string toString(const Location& location);

} // namespace Luau
