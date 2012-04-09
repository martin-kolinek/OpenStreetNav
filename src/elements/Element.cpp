/*
 * Element.cpp
 *
 *  Created on: Nov 29, 2011
 *      Author: martin
 */

#include "Element.h"
#include "HashElementContainer.h"

namespace osm
{

Element::~Element()
{
}

std::shared_ptr<HashElementContainer> Element::get_highlighted() const
{
    return std::shared_ptr<HashElementContainer>(new HashElementContainer());
}

/* namespace osm */
}

