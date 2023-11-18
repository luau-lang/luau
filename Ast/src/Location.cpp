// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/Location.h"

namespace Luau
{

bool Position::operator==(const Position& rhs) const
{
    return this->column == rhs.column && this->line == rhs.line;
}

bool Position::operator!=(const Position& rhs) const
{
    return !(*this == rhs);
}

bool Position::operator<(const Position& rhs) const
{
    if (line == rhs.line)
        return column < rhs.column;
    else
        return line < rhs.line;
}

bool Position::operator>(const Position& rhs) const
{
    if (line == rhs.line)
        return column > rhs.column;
    else
        return line > rhs.line;
}

bool Position::operator<=(const Position& rhs) const
{
    return *this == rhs || *this < rhs;
}

bool Position::operator>=(const Position& rhs) const
{
    return *this == rhs || *this > rhs;
}

void Position::shift(const Position& start, const Position& oldEnd, const Position& newEnd)
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

bool Location::operator==(const Location& rhs) const
{
    return this->begin == rhs.begin && this->end == rhs.end;
}

bool Location::operator!=(const Location& rhs) const
{
    return !(*this == rhs);
}

bool Location::encloses(const Location& l) const
{
    return begin <= l.begin && end >= l.end;
}

bool Location::overlaps(const Location& l) const
{
    return (begin <= l.begin && end >= l.begin) || (begin <= l.end && end >= l.end) || (begin >= l.begin && end <= l.end);
}

bool Location::contains(const Position& p) const
{
    return begin <= p && p < end;
}

bool Location::containsClosed(const Position& p) const
{
    return begin <= p && p <= end;
}

void Location::extend(const Location& other)
{
    if (other.begin < begin)
        begin = other.begin;
    if (other.end > end)
        end = other.end;
}

void Location::shift(const Position& start, const Position& oldEnd, const Position& newEnd)
{
    begin.shift(start, oldEnd, newEnd);
    end.shift(start, oldEnd, newEnd);
}

} // namespace Luau
