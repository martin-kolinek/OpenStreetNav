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

namespace roads
{

class RoadNetwork
{
public:
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
private:
    std::vector<std::unique_ptr<RoadNetworkNode> > nodes;
    std::map<std::pair<RoadNetworkNode const*, RoadNetworkNode const*>, RoadEdgeWithNodes> nodes_to_edges;
    std::multimap<int64_t, RoadNetworkNode*> edge_starts;
    std::multimap<int64_t, RoadNetworkNode*> edge_ends;
};

} /* namespace roads */
#endif /* ROADNETWORK_H_ */
