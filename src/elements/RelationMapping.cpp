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

} /* namespace osm */
