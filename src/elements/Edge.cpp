/*
 * Edge.cpp
 *
 *  Created on: Nov 12, 2011
 *      Author: martin
 */

#include "Edge.h"
#include "ElementContainer.h"

namespace osm
{

Edge::Edge(Node const& start_node, int start_seq_no, Node const& end_node, int end_seq_no, osm::Way const& way):
    start_node(start_node),
    start_seq_no(start_seq_no),
    end_node(end_node),
    end_seq_no(end_seq_no),
    way(way)
{
}

bool Edge::is_intersected(ElementContainer const& c) const
{
    return c.intersects(*this);
}

Edge::~Edge()
{
}

} /* namespace display */
