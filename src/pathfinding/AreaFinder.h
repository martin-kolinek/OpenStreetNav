/*
 * AreaFinder.h
 *
 *  Created on: May 23, 2012
 *      Author: martin
 */

#ifndef AREAFINDER_H_
#define AREAFINDER_H_

#include "../roads/roads.h"
#include "AreaAlgorithm.h"
#include <memory>
#include "Route.h"

namespace pathfind
{

class AreaFinder
{
public:
    AreaFinder(std::shared_ptr<roads::RoadNetwork> const& net, std::shared_ptr<AreaAlgorithm<roads::RoadNetworkNode const*, double> > alg);
    virtual ~AreaFinder();
    Route get_area(osm::Node const& node_id, double max_cost);
private:
    std::shared_ptr<roads::RoadNetwork> const& net;
    std::shared_ptr<AreaAlgorithm<roads::RoadNetworkNode const*, double> > alg;
};

} /* namespace pathfind */
#endif /* AREAFINDER_H_ */
