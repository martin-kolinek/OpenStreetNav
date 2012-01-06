/*
 * LineDisplayStyle.cpp
 *
 *  Created on: Dec 29, 2011
 *      Author: martin
 */

#include "LineDisplayStyle.h"

namespace display
{

LineDisplayStyle::LineDisplayStyle(double red, double green, double blue, double alpha, double thickness):
    red(red),
    green(green),
    blue(blue),
    alpha(alpha),
    thickness(thickness * 0.005)
{
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

}

/* namespace display */
