/*
 * RoadEdge.cpp
 *
 *  Created on: Mar 4, 2012
 *      Author: martin
 */

#include "RoadEdge.h"

namespace roads
{

RoadEdge::RoadEdge(osm::Way const& way, int start_seq_no, int end_seq_no, bool forward, double cost):
    way(way),
    start_seq_no(start_seq_no),
    end_seq_no(end_seq_no),
    forward(forward),
    cost(cost)
{
}

const osm::Way& RoadEdge::get_way() const
{
    return way;
}

osm::Way& RoadEdge::get_way()
{
    return way;
}

int& RoadEdge::get_start_seq_no()
{
    return start_seq_no;
}

const int& RoadEdge::get_start_seq_no() const
{
    return start_seq_no;
}

int& RoadEdge::get_end_seq_no()
{
    return end_seq_no;
}

const int& RoadEdge::get_end_seq_no() const
{
    return end_seq_no;
}

RoadEdge::~RoadEdge()
{
}

} /* namespace roads */
