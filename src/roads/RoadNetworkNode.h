/*
 * RoadNetworkNode.h
 *
 *  Created on: Mar 11, 2012
 *      Author: martin
 */

#ifndef ROADNETWORKNODE_H_
#define ROADNETWORKNODE_H_

#include <vector>
#include "../geoelements/geoelements.h"
#include <cstdint>

namespace roads
{

class RoadNetworkNode
{
public:
    virtual ~RoadNetworkNode();
    geo::Point position;
    std::vector<std::pair<double, RoadNetworkNode const*> > neighbours;
    int64_t nd_id;
};

} /* namespace roads */
#endif /* ROADNETWORKNODE_H_ */
