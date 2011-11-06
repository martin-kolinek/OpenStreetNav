/*
 * Node.cpp
 *
 *  Created on: Nov 5, 2011
 *      Author: martin
 */

#include "Node.h"

namespace osm
{

Node::Node(int64_t id, double lat, double lon):
    id(id),
    lat(lat),
    lon(lon)
{
}

Node::~Node()
{
}

}
