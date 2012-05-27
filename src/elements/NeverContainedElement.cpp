/*
 * NeverContainedElement.cpp
 *
 *  Created on: May 25, 2012
 *      Author: martin
 */

#include "NeverContainedElement.h"

namespace osm
{

bool NeverContainedElement::is_intersected(ElementContainer const&) const
{
    return false;
}

NeverContainedElement::~NeverContainedElement()
{
}

} /* namespace osm */
