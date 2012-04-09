/*
 * ColorStyleChanger.cpp
 *
 *  Created on: Apr 9, 2012
 *      Author: martin
 */

#include "ColorStyleChanger.h"

namespace display
{

ColorStyleChanger::ColorStyleChanger(double red, double green, double blue, double alpha, double thickness_add):
    red(red),
    green(green),
    blue(blue),
    alpha(alpha),
    thickness_add(thickness_add)
{
}

ColorStyleChanger::~ColorStyleChanger()
{
}

std::shared_ptr<DisplayStyle> ColorStyleChanger::visit(LineDisplayStyle const& s) const
{
    return std::shared_ptr<DisplayStyle>(new LineDisplayStyle(red, green, blue, alpha, s.get_thickness() + thickness_add, s.arrow));
}

} /* namespace display */
