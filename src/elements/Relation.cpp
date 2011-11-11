/*
 * Relation.cpp
 *
 *  Created on: Nov 5, 2011
 *      Author: martin
 */

#include "Relation.h"

namespace osm
{

Relation::Relation()
{
}

Relation::Relation(int64_t id):
    id(id)
{
}

Relation::~Relation()
{
}

bool Relation::operator==(Relation const& other) const
{
    return id == other.id && members == other.members && tags == other.tags;
}

bool Relation::operator!=(Relation const& other) const
{
    return !(*this == other);
}

}
