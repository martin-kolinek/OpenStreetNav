/*
 * PathFinder.h
 *
 *  Created on: Mar 21, 2012
 *      Author: martin
 */

#ifndef PATHFINDER_H_
#define PATHFINDER_H_

#include "../osmdb/osmdb.h"
#include "../roads/roads.h"
#include "Route.h"
#include "PathFindingAlgorithm.h"

namespace pathfind
{

class PathFinder
{
public:
    PathFinder(std::shared_ptr<roads::RoadNetwork> const& net, std::shared_ptr<PathFindingAlgorithm<roads::RoadNetworkNode const*> > alg);
    Route find_way(osm::Node const& start_node, osm::Node const& end_node);
    virtual ~PathFinder();
private:
    std::shared_ptr<roads::RoadNetwork> net;
    std::shared_ptr<PathFindingAlgorithm<roads::RoadNetworkNode const*> > alg;
};

} /* namespace pathfind */
#endif /* PATHFINDER_H_ */
