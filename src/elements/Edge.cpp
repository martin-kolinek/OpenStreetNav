/*
 * Edge.cpp
 *
 *  Created on: Nov 12, 2011
 *      Author: martin
 */

#include "Edge.h"

namespace osm
{

Edge::Edge(Node const& start_node, Node const& end_node, osm::Way const& way):
    start_node(start_node),
    end_node(end_node),
    way(way)
{
}

Edge::~Edge()
{
}

} /* namespace display */
