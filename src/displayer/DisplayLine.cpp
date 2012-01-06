/*
 * DisplayLine.cpp
 *
 *  Created on: Dec 29, 2011
 *      Author: martin
 */

#include "DisplayLine.h"

namespace display
{

DisplayLine::DisplayLine(geo::Point p1, geo::Point p2, std::unique_ptr<DisplayStyle> && style):
    DisplayElement(std::move(style)),
    p1(p1),
    p2(p2)
{
}

DisplayLine::~DisplayLine()
{
}

DisplayElementType DisplayLine::get_type() const
{
    return DisplayElementType::Line;
}

bool DisplayLine::operator <(const DisplayElement& other) const
{
    if (other.get_type() != DisplayElementType::Line)
        return DisplayElementType::Line < other.get_type();
    DisplayLine const& ln = static_cast<DisplayLine const&>(other);
    return std::pair<geo::Point, geo::Point>(p1, p2) < std::pair<geo::Point, geo::Point>(ln.p1, ln.p2);
}

bool DisplayLine::operator >(const DisplayElement& other) const
{
    return !(*this == other) && !(*this < other);
}

bool DisplayLine::operator <=(const DisplayElement& other) const
{
    return *this == other || *this < other;
}

bool DisplayLine::operator >=(const DisplayElement& other) const
{
    return !(*this < other);
}

bool DisplayLine::operator ==(const DisplayElement& other) const
{
    if (other.get_type() != DisplayElementType::Line)
        return false;
    DisplayLine const& ln = static_cast<DisplayLine const&>(other);
    return std::pair<geo::Point, geo::Point>(p1, p2) == std::pair<geo::Point, geo::Point>(ln.p1, ln.p2);
}

bool DisplayLine::operator !=(const DisplayElement& other) const
{
    return !(*this == other);
}

void DisplayLine::draw_internal(Cairo::RefPtr<Cairo::Context> cr, proj::MapProjection& pr) const
{
    auto p = pr.project(p1);
    cr->move_to(p.x, p.y);
    p = pr.project(p2);
    cr->line_to(p.x, p.y);
}

/* namespace display */
}

