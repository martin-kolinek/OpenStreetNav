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

bool Node::operator==(const Node& other) const
{
    return id == other.id && lat == other.lat && lon == other.lon && tags == other.tags;
}

const std::vector<Tag> & Node::get_tags()
{
    return tags;
}

int64_t Node::get_id()
{
    return id;
}

std::string Node::get_type_str()
{
    return "node";
}

bool Node::operator !=(const Node& other) const
{
    return !(*this == other);
}

}
