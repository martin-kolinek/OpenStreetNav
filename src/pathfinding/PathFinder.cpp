/*
 * PathFinder.cpp
 *
 *  Created on: Mar 21, 2012
 *      Author: martin
 */

#include "PathFinder.h"
#include "AStar.h"
#include <cmath>

namespace pathfind
{

PathFinder::PathFinder(osmdb::RoadLister& db, std::shared_ptr<PathFindingAlgorithm<roads::RoadNetworkNode const*> > alg):
    alg(alg)
{
    auto v = db.get_edges();
    for (auto it = v.begin(); it != v.end(); ++it)
    {
        net.add_edge(*it);
    }
}

Route PathFinder::find_way(const osm::Node& start_node, const osm::Node& end_node)
{

    auto cxt = net.get_path_find_context(start_node.id, end_node.id);
    if (!cxt.is_valid())
        return Route();
    auto v = alg->find_path(cxt.get_start_nodes(), cxt.get_end_nodes());
    auto v2 = net.resolve_path(v);
    return Route(v2);
}

PathFinder::~PathFinder()
{
}

} /* namespace pathfind */
