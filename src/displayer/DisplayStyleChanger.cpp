/*
 * DisplayStyleChanger.cpp
 *
 *  Created on: Apr 9, 2012
 *      Author: martin
 */

#include "DisplayStyleChanger.h"

namespace display
{
std::shared_ptr<DisplayStyle> DisplayStyleChanger::visit(const LineDisplayStyle& s) const
{
    return s.copy();
}

std::shared_ptr<DisplayStyle> DisplayStyleChanger::visit(const DisplayStyle& s) const
{
    return s.copy();
}

DisplayStyleChanger::~DisplayStyleChanger()
{
}

} /* namespace display */
