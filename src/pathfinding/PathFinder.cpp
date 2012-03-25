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

PathFinder::PathFinder(osmdb::RoadLister& db)
{
    auto v = db.get_edges();
    for (auto it = v.begin(); it != v.end(); ++it)
    {
        net.add_edge(*it);
    }
}

double get_cost_estimate(roads::RoadNetworkNode const* r, roads::PathFindContext const& cxt)
{
    double ret = std::numeric_limits<double>::infinity();
    auto v = cxt.get_end_nodes();
    for (auto it = v.begin(); it != v.end(); ++it)
    {
        ret = std::min(ret, geo::get_point_distance(EARTH_RADIUS, (*it)->position, r->position));
    }
    return ret;
}

Route PathFinder::find_way(const osm::Node& start_node, const osm::Node& end_node)
{

    auto cxt = net.get_path_find_context(start_node.id, end_node.id);
    if (!cxt.is_valid())
        return Route();
    auto v = find_path(cxt.get_start_node(), 0.0,
                       [](roads::RoadNetworkNode const * r)
    {
        return r->neighbours;
    },
    [&](roads::RoadNetworkNode const * r)
    {
        return get_cost_estimate(r, cxt);
    },
    [&](roads::RoadNetworkNode const * r)
    {
        return cxt.is_end_node(r);
    }
                      );

    auto v2 = net.resolve_path(v);
    return Route(v2);
}

PathFinder::~PathFinder()
{
}

} /* namespace pathfind */
