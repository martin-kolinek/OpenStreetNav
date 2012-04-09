/*
 * DisplayLine.cpp
 *
 *  Created on: Dec 29, 2011
 *      Author: martin
 */

#include "DisplayLine.h"

namespace display
{

DisplayLine::DisplayLine(osm::Edge const& edge, std::shared_ptr<DisplayStyle> style):
    style(std::move(style)),
    edge(edge)
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
    return std::pair<geo::Point, geo::Point>(edge.get_start_node().position, edge.get_end_node().position) < std::pair<geo::Point, geo::Point>(ln.edge.get_start_node().position, ln.edge.get_end_node().position);
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
    return std::pair<geo::Point, geo::Point>(edge.get_start_node().position, edge.get_end_node().position) == std::pair<geo::Point, geo::Point>(ln.edge.get_start_node().position, ln.edge.get_end_node().position);
}

bool DisplayLine::operator !=(const DisplayElement& other) const
{
    return !(*this == other);
}

void DisplayLine::draw_internal(Cairo::RefPtr<Cairo::Context> cr, proj::MapProjection& pr) const
{
    auto pp1 = pr.project(edge.get_start_node().position);
    auto pp2 = pr.project(edge.get_end_node().position);
    if (style->draw_arrow() != ArrowStyle::None)
    {
        double l = sqrt((pp2.x - pp1.x) * (pp2.x - pp1.x) + (pp2.y - pp1.y) * (pp2.y - pp1.y));
        double norm_x = (pp2.x - pp1.x) / (l * 100);
        double norm_y = (pp2.y - pp1.y) / (l * 100);
        if (style->draw_arrow() != ArrowStyle::Forward)
        {
            norm_x = -norm_x;
            norm_y = -norm_y;
        }
        if (isnan(norm_x))
            norm_x = 0;
        if (isnan(norm_y))
            norm_y = 0;
        proj::FlatPoint ppc((pp1.x + pp2.x) / 2.0, (pp1.y + pp2.y) / 2.0);
        cr->move_to(pp1.x, pp1.y);
        cr->line_to(ppc.x, ppc.y);
        cr->line_to(ppc.x - norm_x - norm_y, ppc.y - norm_y + norm_x);
        cr->move_to(ppc.x, ppc.y);
        cr->line_to(ppc.x - norm_x + norm_y, ppc.y - norm_y - norm_x);
        cr->move_to(ppc.x, ppc.y);
        cr->line_to(pp2.x, pp2.y);
    }
    else
    {
        cr->move_to(pp1.x, pp1.y);
        cr->line_to(pp2.x, pp2.y);
    }
}

DisplayStyle const& DisplayLine::get_style() const
{
    return *style;
}

osm::Edge const& DisplayLine::get_edge() const
{
    return edge;
}

/* namespace display */
}

