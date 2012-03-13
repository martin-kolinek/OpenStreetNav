#include "PathFindContext.h"
#include "../util/tuple_sub.h"

namespace roads
{

RoadNetworkNode const* PathFindContext::get_start_node() const
{
    return start_node.get();
}

bool PathFindContext::is_end_node(RoadNetworkNode const* ptr) const
{
    return std::binary_search(end_nodes.begin(), end_nodes.end(), ptr);
}

}
