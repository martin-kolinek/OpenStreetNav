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

namespace pathfind
{

class PathFinder
{
public:
    PathFinder(osmdb::RoadLister& db);
    Route find_way(osm::Node const& start_node, osm::Node const& end_node);
    virtual ~PathFinder();
private:
    roads::RoadNetwork net;
};

} /* namespace pathfind */
#endif /* PATHFINDER_H_ */
