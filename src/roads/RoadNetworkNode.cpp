/*
 * RoadNetworkNode.cpp
 *
 *  Created on: Mar 11, 2012
 *      Author: martin
 */

#include "RoadNetworkNode.h"

namespace roads
{

RoadNetworkNode::~RoadNetworkNode()
{
}

std::vector<std::pair<double, IRoadNetworkNode const*> > const& RoadNetworkNode::get_neighbours() const
{
    return neighbours;
}

} /* namespace roads */
