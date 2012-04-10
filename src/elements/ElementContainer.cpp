/*
 * ElementContainer.cpp
 *
 *  Created on: Apr 9, 2012
 *      Author: martin
 */

#include "ElementContainer.h"

namespace osm
{

bool ElementContainer::intersects(const Edge&) const
{
    return false;
}

ElementContainer::~ElementContainer()
{
}

} /* namespace display */
