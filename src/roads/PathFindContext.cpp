#include "PathFindContext.h"
#include "../util/tuple_sub.h"

namespace roads
{

bool PathFindContext::is_valid()
{
    return start_nodes.size() > 0 && end_nodes.size() > 0;
}

std::vector<RoadNetworkNode const*> const& PathFindContext::get_start_nodes() const
{
    return start_nodes;
}

std::vector<RoadNetworkNode const*> const& PathFindContext::get_end_nodes() const
{
    return end_nodes;
}

}
