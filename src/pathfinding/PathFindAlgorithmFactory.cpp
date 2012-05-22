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

namespace pathfind
{

double get_cost_estimate(roads::RoadNetworkNode const* r, std::vector<roads::RoadNetworkNode const*> const& end)
{
    auto dists = end | util::selected([&r](roads::RoadNetworkNode const * r2)
    {
        return geo::get_point_distance(EARTH_RADIUS, r2->position, r->position);
    });
    //auto i = boost::range::min_element(dists);
    return *dists.begin();
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
    });
}

} /* namespace pathfind */
