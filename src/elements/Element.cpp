/*
 * Element.cpp
 *
 *  Created on: Nov 29, 2011
 *      Author: martin
 */

#include "Element.h"

namespace osm
{

Element::~Element()
{
}

bool Element::operator !=(const Element& e) const
{
    return !(*this == e);
}

/* namespace osm */
}

