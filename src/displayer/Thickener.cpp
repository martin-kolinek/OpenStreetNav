/*
 * Thickener.cpp
 *
 *  Created on: Apr 11, 2012
 *      Author: martin
 */

#include "Thickener.h"

namespace display
{

Thickener::Thickener(double addition):
    addition(addition)
{
}

Thickener::~Thickener()
{
}

std::shared_ptr<DisplayStyle> Thickener::visit(LineDisplayStyle const& s) const
{
    return std::shared_ptr<DisplayStyle>(new LineDisplayStyle(s.red, s.green, s.blue, s.alpha, s.get_thickness() + addition, s.arrow));
}

} /* namespace osm */
