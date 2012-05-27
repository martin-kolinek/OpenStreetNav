/*
 * DisplayElement.cpp
 *
 *  Created on: Dec 7, 2011
 *      Author: martin
 */

#include "DisplayElement.h"

namespace display
{

DisplayElement::DisplayElement(std::shared_ptr<DisplayStyle> style):
    style(style)
{
}

DisplayElement::~DisplayElement()
{
}

bool DisplayElement::operator >(const DisplayElement& other) const
{
    return !(*this == other) && !(*this < other);
}

bool DisplayElement::operator <=(const DisplayElement& other) const
{
    return *this == other || *this < other;
}

bool DisplayElement::operator >=(const DisplayElement& other) const
{
    return !(*this < other);
}

bool DisplayElement::operator !=(const DisplayElement& other) const
{
    return !(*this == other);
}

DisplayStyle const& DisplayElement::get_style() const
{
    return *style;
}

void DisplayElement::draw(Cairo::RefPtr<Cairo::Context> cr, proj::MapProjection& pr) const
{
    get_style().prepare(cr);
    draw_internal(cr, pr);
    get_style().exec(cr);
}

}    /* namespace display */


