/*
 * RoadNetworkNode.h
 *
 *  Created on: Mar 11, 2012
 *      Author: martin
 */

#ifndef ROADNETWORKNODE_H_
#define ROADNETWORKNODE_H_

#include <vector>
#include "IRoadNetworkNode.h"

namespace roads
{

class RoadNetworkNode : public IRoadNetworkNode
{
public:
    virtual ~RoadNetworkNode();
    std::vector<std::pair<double, IRoadNetworkNode const*> > const& get_neighbours() const;
    std::vector<std::pair<double, IRoadNetworkNode const*> > neighbours;
};

} /* namespace roads */
#endif /* ROADNETWORKNODE_H_ */
