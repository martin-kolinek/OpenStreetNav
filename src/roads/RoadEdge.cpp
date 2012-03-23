/*
 * RoadEdge.cpp
 *
 *  Created on: Mar 4, 2012
 *      Author: martin
 */

#include "RoadEdge.h"

namespace roads
{

RoadEdge::RoadEdge(int64_t way_id, int start_seq_no, int end_seq_no, bool forward, double cost):
    way_id(way_id),
    start_seq_no(start_seq_no),
    end_seq_no(end_seq_no),
    forward(forward),
    cost(cost)
{
}

RoadEdge::~RoadEdge()
{
}

} /* namespace roads */
