#ifndef PATHFINDCONTEXT_H_
#define PATHFINDCONTEXT_H_

#include "RoadNetworkNode.h"
#include <vector>
#include <memory>
#include <algorithm>

namespace roads
{

class PathFindContext
{
public:
    template<typename Rng1, typename Rng2>
    PathFindContext(Rng1 const& start_nodes, Rng2 const& end_nodes)
    {
        for (auto it = start_nodes.begin(); it != start_nodes.end(); ++it)
        {
            this->start_nodes.push_back(*it);
        }

        for (auto it = end_nodes.begin(); it != end_nodes.end(); ++it)
        {
            this->end_nodes.push_back(*it);
        }
    }
    bool is_valid();
    std::vector<RoadNetworkNode const*> const& get_start_nodes() const;
    std::vector<RoadNetworkNode const*> const& get_end_nodes() const;
private:
    std::vector<RoadNetworkNode const*> start_nodes;
    std::vector<RoadNetworkNode const*> end_nodes;
};

}

#endif
