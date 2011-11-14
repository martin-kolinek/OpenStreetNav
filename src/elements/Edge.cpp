/*
 * Edge.cpp
 *
 *  Created on: Nov 12, 2011
 *      Author: martin
 */

#include "Edge.h"

namespace osm
{

Edge::Edge(int64_t start_node, int64_t end_node, int64_t way_id):
    start_node(start_node),
    end_node(end_node),
    way_id(way_id)
{
}

Edge::~Edge()
{
}

} /* namespace display */
