/*
 * RelationMapping.cpp
 *
 *  Created on: Nov 5, 2011
 *      Author: martin
 */

#include "RelationMapping.h"

namespace osm
{

RelationMapping::RelationMapping(std::string const& role, ObjectType type, int64_t id):
    role(role),
    type(type),
    id(id)
{
}

RelationMapping::~RelationMapping()
{
}

bool RelationMapping::operator==(RelationMapping const& other) const
{
    return role == other.role && id == other.id && type == other.type;
}

bool RelationMapping::operator!=(RelationMapping const& other) const
{
    return !(*this == other);
}

} /* namespace osm */
