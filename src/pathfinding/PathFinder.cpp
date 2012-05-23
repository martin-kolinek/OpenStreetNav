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

PathFinder::PathFinder(std::shared_ptr<roads::RoadNetwork> const& net, std::shared_ptr<PathFindingAlgorithm<roads::RoadNetworkNode const*> > alg):
    net(net),
    alg(alg)
{
}

Route PathFinder::find_way(const osm::Node& start_node, const osm::Node& end_node)
{

    auto cxt = net->get_path_find_context(start_node.id, end_node.id);
    if (!cxt.is_valid())
        return Route();
    auto v = alg->find_path(cxt.get_start_nodes(), cxt.get_end_nodes());
    auto v2 = net->resolve_path(v);
    return Route(v2);
}

PathFinder::~PathFinder()
{
}

} /* namespace pathfind */
