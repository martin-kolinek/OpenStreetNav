/*
 * LineDisplayStyle.cpp
 *
 *  Created on: Dec 29, 2011
 *      Author: martin
 */

#include "LineDisplayStyle.h"
#include "DisplayStyleChanger.h"

namespace display
{

LineDisplayStyle::LineDisplayStyle(double red, double green, double blue, double alpha, double thickness, bool arrow):
    red(red),
    green(green),
    blue(blue),
    alpha(alpha),
    arrow(arrow),
    thickness(thickness * 0.005)
{
}

bool LineDisplayStyle::draw_arrow() const
{
    return arrow;
}

LineDisplayStyle::~LineDisplayStyle()
{
}

void LineDisplayStyle::prepare(Cairo::RefPtr<Cairo::Context> cr) const
{
    cr->set_source_rgba(red, green, blue, alpha);
    cr->set_line_width(thickness);
    cr->set_line_cap(Cairo::LineCap::LINE_CAP_SQUARE);
}

void LineDisplayStyle::exec(Cairo::RefPtr<Cairo::Context> cr) const
{
    cr->stroke();
}

std::shared_ptr<DisplayStyle> LineDisplayStyle::copy() const
{
    return std::shared_ptr<DisplayStyle>(new LineDisplayStyle(*this));
}

std::shared_ptr<DisplayStyle> LineDisplayStyle::accept(const DisplayStyleChanger& c) const
{
    return c.visit(*this);
}

double LineDisplayStyle::get_thickness() const
{
    return thickness / 0.005;
}

} /* namespace display */

