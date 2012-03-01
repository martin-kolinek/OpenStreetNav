/*
 * DisplayElement.cpp
 *
 *  Created on: Dec 7, 2011
 *      Author: martin
 */

#include "DisplayElement.h"

namespace display
{

DisplayElement::DisplayElement()
{
}

DisplayElement::~DisplayElement()
{
}

void DisplayElement::draw(Cairo::RefPtr<Cairo::Context> cr, proj::MapProjection& pr) const
{
    get_style().prepare(cr);
    draw_internal(cr, pr);
    get_style().exec(cr);
}

}    /* namespace display */


