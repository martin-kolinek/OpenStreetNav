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

void DisplayLine::draw_internal(Cairo::RefPtr<Cairo::Context> cr, proj::MapProjection& pr) const
{
    auto p = pr.project(p1);
    cr->move_to(p.x, p.y);
    p = pr.project(p2);
    cr->line_to(p.x, p.y);
}

}

/* namespace display */
