#include "PathFindContext.h"
#include "../util/tuple_sub.h"

namespace roads
{

IRoadNetworkNode const* PathFindContext::get_start_node() const
{
    return start_node.get();
}

bool PathFindContext::is_end_node(IRoadNetworkNode const* ptr) const
{
    return std::binary_search(end_nodes.begin(), end_nodes.end(), ptr);
}

}
