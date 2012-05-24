/*
 * RoadNetwork.h
 *
 *  Created on: Mar 11, 2012
 *      Author: martin
 */

#ifndef ROADNETWORK_H_
#define ROADNETWORK_H_

#include "RoadEdgeWithNodes.h"
#include "RoadNetworkNode.h"
#include "PathFindContext.h"
#include <vector>
#include <map>
#include <memory>
#include <unordered_set>
#include <iostream>

namespace roads
{

class RoadNetwork
{
public:
    RoadNetwork();
    virtual ~RoadNetwork();
    void add_edge(RoadEdgeWithNodes const& e);
    PathFindContext get_path_find_context(int64_t start_id, int64_t end_id);
    template<typename Rng>
    std::vector<RoadEdgeWithNodes> resolve_path(Rng const& rng)
    {
        std::vector<RoadEdgeWithNodes> ret;
        for (auto it = rng.begin(); it != rng.end(); ++it)
        {
            auto it2 = it;
            ++it2;
            if (it2 == rng.end())
            {
                continue;
            }

            auto mit = nodes_to_edges.find(std::make_pair(*it, *it2));
            if (mit != nodes_to_edges.end())
                ret.push_back(mit->second);
        }
        return ret;
    }
    template<typename Rng>
    std::vector<RoadEdgeWithNodes>  resolve_area(Rng const& rng)
    {
        std::unordered_set<RoadNetworkNode const*> set;
        for (auto it = rng.begin(); it != rng.end(); ++it)
        {
            set.insert(*it);
        }
        std::vector<RoadEdgeWithNodes> ret;

        for (auto it = rng.begin(); it != rng.end(); ++it)
        {
            auto it2 = nodes_to_edges.lower_bound(std::make_pair(*it, min_ptr));
            if (it2 != nodes_to_edges.end() && it2->first.first != *it)
                ++it2;
            for (; it2 != nodes_to_edges.end() && it2->first.first == *it; ++it2)
            {
                if (set.find(it2->first.first) != set.end() && set.find(it2->first.second) != set.end())
                    ret.push_back(it2->second);
            }
        }

        return ret;
    }
private:
    std::vector<std::unique_ptr<RoadNetworkNode> > nodes;
    std::map<std::pair<RoadNetworkNode const*, RoadNetworkNode const*>, RoadEdgeWithNodes> nodes_to_edges;
    std::multimap<int64_t, RoadNetworkNode*> edge_starts;
    std::multimap<int64_t, RoadNetworkNode*> edge_ends;
    RoadNetworkNode const* min_ptr;
};

} /* namespace roads */
#endif /* ROADNETWORK_H_ */
