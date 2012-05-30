/*
 * PathFindAlgorithmFactory.cpp
 *
 *  Created on: May 21, 2012
 *      Author: martin
 */

#include "PathFindAlgorithmFactory.h"

#include <boost/range/algorithm.hpp>
#include "../util/range.h"
#include "AStarPathFinding.h"
#include "AStarAreaFinding.h"

namespace pathfind
{

double get_cost_estimate(roads::RoadNetworkNode const* r, std::vector<roads::RoadNetworkNode const*> const& end)
{
    auto dists = end | util::selected([&r](roads::RoadNetworkNode const * r2)
    {
        return geo::get_point_distance(EARTH_RADIUS, r2->position, r->position);
    });
    auto it = dists.begin();
    double min = *it;
    for (; it != dists.end(); ++it)
    {
        if (*it < min)
            min = *it;
    }
    return min;

}

std::shared_ptr<PathFindingAlgorithm<roads::RoadNetworkNode const*> > PathFindAlgorithmFactory::get_dijkstra_algorithm(int step)
{
    return get_astar<roads::RoadNetworkNode const*, double>(step,
            [](roads::RoadNetworkNode const * r)
    {
        return r->neighbours;
    },
    [](roads::RoadNetworkNode const*, std::vector<roads::RoadNetworkNode const*> const&)
    {
        return 0;
    },
    [](roads::RoadNetworkNode const*, roads::RoadNetworkNode const*)
    {
        return true;
    });
}

std::shared_ptr<PathFindingAlgorithm<roads::RoadNetworkNode const*> > PathFindAlgorithmFactory::get_astar_algorithm(int step)
{
    return get_astar<roads::RoadNetworkNode const*, double>(step,
            [](roads::RoadNetworkNode const * r)
    {
        return r->neighbours;
    },
    [](roads::RoadNetworkNode const * r, std::vector<roads::RoadNetworkNode const*> const & end)
    {
        return get_cost_estimate(r, end);
    },
    [](roads::RoadNetworkNode const * p1, roads::RoadNetworkNode const * p2)
    {
        return p1->position == p2->position;
    });
}

std::shared_ptr<AreaAlgorithm<roads::RoadNetworkNode const*, double> > PathFindAlgorithmFactory::get_astar_area_algorithm(int step)
{
    return get_astar_area<roads::RoadNetworkNode const*, double>(step,
            [](roads::RoadNetworkNode const * r)
    {
        return r->neighbours;
    });
}

} /* namespace pathfind */
