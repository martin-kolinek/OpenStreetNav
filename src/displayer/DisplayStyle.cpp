/*
 * DisplayStyle.cpp
 *
 *  Created on: Dec 7, 2011
 *      Author: martin
 */

#include "DisplayStyle.h"

namespace display
{

DisplayStyle::~DisplayStyle()
{
}

ArrowStyle DisplayStyle::draw_arrow() const
{
    return ArrowStyle::None;
}

} /* namespace display */
