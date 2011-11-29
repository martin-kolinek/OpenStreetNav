/*
 * Way.cpp
 *
 *  Created on: Nov 5, 2011
 *      Author: martin
 */

#include "Way.h"

namespace osm
{

Way::Way()
{
}

Way::Way(int64_t id):
    id(id)
{
}

Way::~Way()
{
}

bool Way::operator==(const Way& other) const
{
    return id == other.id && nodes == other.nodes && tags == other.tags;
}

const std::vector<Tag> & Way::get_tags()
{
    return tags;
}

int64_t Way::get_id()
{
    return id;
}

std::string Way::get_type_str()
{
    return "way";
}

bool Way::operator !=(const Way& other) const
{
    return !(*this == other);
}

}
