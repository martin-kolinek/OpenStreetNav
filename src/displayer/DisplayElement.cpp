/*
 * DisplayElement.cpp
 *
 *  Created on: Dec 7, 2011
 *      Author: martin
 */

#include "DisplayElement.h"

namespace display
{



DisplayElement::DisplayElement(std::unique_ptr<DisplayStyle> disp):
    disp(std::move(disp))
{
}

DisplayElement::~DisplayElement()
{
}

void DisplayElement::draw(Cairo::RefPtr<Cairo::Context> cr, proj::MapProjection& pr)
{
    disp->prepare(cr);
    draw_internal(cr, pr);
    disp->exec(cr);
}

}

/* namespace display */
