/*
 * RoadEdgeWithNodes.cpp
 *
 *  Created on: Mar 11, 2012
 *      Author: martin
 */

#include "RoadEdgeWithNodes.h"

namespace roads
{

RoadEdgeWithNodes::RoadEdgeWithNodes(int64_t way_id, int start_seq_no, int end_seq_no, bool forward, double cost, osm::Node start_node, osm::Node end_node):
    RoadEdge(way_id, start_seq_no, end_seq_no, forward, cost),
    start_node(start_node),
    end_node(end_node)
{
}

RoadEdgeWithNodes::~RoadEdgeWithNodes()
{
}

osm::Node& RoadEdgeWithNodes::get_start_node()
{
    return start_node;
}

osm::Node& RoadEdgeWithNodes::get_end_node()
{
    return end_node;
}

const osm::Node& RoadEdgeWithNodes::get_start_node() const
{
    return start_node;
}

const osm::Node& RoadEdgeWithNodes::get_end_node() const
{
    return end_node;
}

} /* namespace roads */
