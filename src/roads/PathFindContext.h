#ifndef PATHFINDCONTEXT_H_
#define PATHFINDCONTEXT_H_

#include "IRoadNetworkNode.h"
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
        std::unique_ptr<RoadNetworkNode> ptr(new RoadNetworkNode());
        for (auto it = start_nodes.begin(); it != start_nodes.end(); ++it)
        {
            ptr->neighbours.push_back(std::make_pair(0, *it));
        }
        start_node = std::shared_ptr<IRoadNetworkNode const>(std::move(ptr));

        for (auto it = end_nodes.begin(); it != end_nodes.end(); ++it)
        {
            this->end_nodes.push_back(*it );
        }
        std::sort(this->end_nodes.begin(), this->end_nodes.end());
    }
    IRoadNetworkNode const* get_start_node() const;
    bool is_end_node(IRoadNetworkNode const* ptr) const;
private:
    std::shared_ptr<IRoadNetworkNode const> start_node;
    std::vector<IRoadNetworkNode const*> end_nodes;
};

}

#endif
