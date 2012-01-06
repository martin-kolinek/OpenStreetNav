/*
 * DisplayLine.cpp
 *
 *  Created on: Dec 29, 2011
 *      Author: martin
 */

#include "DisplayLine.h"

namespace display
{

DisplayLine::DisplayLine(geo::Point p1, geo::Point p2, bool arrow, std::unique_ptr<DisplayStyle> && style):
    DisplayElement(std::move(style)),
    p1(p1),
    p2(p2),
    arrow(arrow)
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
    auto pp1 = pr.project(p1);
    auto pp2 = pr.project(p2);
    if (arrow)
    {
        double l = sqrt((pp2.x - pp1.x) * (pp2.x - pp1.x) + (pp2.y - pp1.y) * (pp2.y - pp1.y));
        double norm_x = (pp2.x - pp1.x) / (l * 100);
        double norm_y = (pp2.y - pp1.y) / (l * 100);
        proj::FlatPoint ppc((pp1.x + pp2.x) / 2.0, (pp1.y + pp2.y) / 2.0);
        cr->move_to(pp1.x, pp1.y);
        cr->line_to(ppc.x, ppc.y);
        cr->line_to(ppc.x - norm_x - norm_y, ppc.y - norm_y + norm_x);
        cr->move_to(ppc.x - norm_x + norm_y, ppc.y - norm_y - norm_x);
        cr->line_to(ppc.x, ppc.y);
        cr->line_to(pp2.x, pp2.y);
    }
    else
    {
        cr->move_to(pp1.x, pp1.y);
        cr->line_to(pp2.x, pp2.y);
    }
}

/* namespace display */
}

