/*
 * RoadEdgeWithNodes.cpp
 *
 *  Created on: Mar 11, 2012
 *      Author: martin
 */

#include "RoadEdgeWithNodes.h"

namespace roads
{

RoadEdgeWithNodes::RoadEdgeWithNodes(int64_t way_id, int seq_no, bool forward, double cost, int64_t start_node_id, int64_t end_node_id):
    RoadEdge(way_id, seq_no, forward, cost),
    start_node_id(start_node_id),
    end_node_id(end_node_id)
{
}

RoadEdgeWithNodes::~RoadEdgeWithNodes()
{
}

} /* namespace roads */
