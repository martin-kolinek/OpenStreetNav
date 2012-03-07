/*
 * RoadEdge.cpp
 *
 *  Created on: Mar 4, 2012
 *      Author: martin
 */

#include "RoadEdge.h"

namespace roads
{

RoadEdge::RoadEdge(int64_t way_id, int seq_no, bool forward, double cost):
    way_id(way_id),
    seq_no(seq_no),
    forward(forward),
    cost(cost)
{
}

RoadEdge::~RoadEdge()
{
}

} /* namespace roads */
