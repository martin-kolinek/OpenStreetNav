/*
 * AreaFinder.cpp
 *
 *  Created on: May 23, 2012
 *      Author: martin
 */

#include "AreaFinder.h"

namespace pathfind
{

AreaFinder::AreaFinder(std::shared_ptr<roads::RoadNetwork> const& net, std::shared_ptr<AreaAlgorithm<roads::RoadNetworkNode const*, double> > alg):
    net(net),
    alg(alg)
{
}

Route AreaFinder::get_area(osm::Node const& node, double max_cost)
{
    auto cxt = net->get_path_find_context(node.id, node.id);
    if (!cxt.is_valid())
        return Route();
    auto v = alg->find_area(cxt.get_start_nodes(), max_cost);
    auto v2 = net->resolve_area(v);
    return Route(v2);
}

AreaFinder::~AreaFinder()
{
}

} /* namespace pathfind */
