/*
 * PureDisplayLine.cpp
 *
 *  Created on: May 25, 2012
 *      Author: martin
 */

#include "PureDisplayLine.h"

namespace display
{

PureDisplayLine::PureDisplayLine(geo::Point  const& p1, geo::Point const& p2, std::shared_ptr<DisplayStyle> style):
    DisplayElement(style),
    p1(p1),
    p2(p2)
{

}

DisplayElementType PureDisplayLine::get_type() const
{
    return DisplayElementType::PureLine;
}

bool PureDisplayLine::operator <(const DisplayElement& other) const
{
    if (other.get_type() != DisplayElementType::PureLine)
        return DisplayElementType::PureLine < other.get_type();
    PureDisplayLine const& ln = static_cast<PureDisplayLine const&>(other);
    return std::make_pair(p1, p2) < std::make_pair(ln.p1, ln.p2);
}

bool PureDisplayLine::operator ==(const DisplayElement& other) const
{
    if (other.get_type() != DisplayElementType::PureLine)
        return false;
    PureDisplayLine const& ln = static_cast<PureDisplayLine const&>(other);
    return std::make_pair(p1, p2) == std::make_pair(ln.p1, ln.p2);
}

PureDisplayLine::~PureDisplayLine()
{
}

osm::ContainedElement const& PureDisplayLine::get_element() const
{
    return el;
}

std::shared_ptr<DisplayElement> PureDisplayLine::copy(std::shared_ptr<DisplayStyle> st)
{
    return std::make_shared<PureDisplayLine>(p1, p2, st);
}

void PureDisplayLine::draw_internal(Cairo::RefPtr<Cairo::Context> cr,
                                    proj::MapProjection& pr) const
{
    auto pp1 = pr.project(p1);
    auto pp2 = pr.project(p2);
    cr->move_to(pp1.x, pp1.y);
    cr->line_to(pp2.x, pp2.y);
}

} /* namespace display */
