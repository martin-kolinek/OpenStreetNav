/*
 * Way.cpp
 *
 *  Created on: Nov 5, 2011
 *      Author: martin
 */

#include "Way.h"

namespace osm
{

Way::Way(int64_t id):
    id(id)
{
}

Way::~Way()
{
}

bool Way::operator==(Way const& other) const
{
    return id == other.id && nodes == other.nodes && tags == other.tags;
}

bool Way::operator!=(Way const& other) const
{
    return !(*this == other);
}

}
