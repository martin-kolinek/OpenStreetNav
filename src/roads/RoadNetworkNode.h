/*
 * RoadNetworkNode.h
 *
 *  Created on: Mar 11, 2012
 *      Author: martin
 */

#ifndef ROADNETWORKNODE_H_
#define ROADNETWORKNODE_H_

#include <vector>

namespace roads
{

class RoadNetworkNode
{
public:
    virtual ~RoadNetworkNode();
    std::vector<std::pair<double, RoadNetworkNode const*> > neighbours;
};

} /* namespace roads */
#endif /* ROADNETWORKNODE_H_ */
