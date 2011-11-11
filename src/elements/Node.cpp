/*
 * Node.cpp
 *
 *  Created on: Nov 5, 2011
 *      Author: martin
 */

#include "Node.h"

namespace osm
{

Node::Node()
{
}

Node::Node(int64_t id, double lat, double lon):
    id(id),
    lat(lat),
    lon(lon)
{
}

Node::~Node()
{
}

bool Node::operator==(Node const& other) const
{
    return id == other.id && lat == other.lat && lon == other.lon && tags == other.tags;
}

bool Node::operator!=(Node const& other) const
{
    return !(*this == other);
}

}
