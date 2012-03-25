/*
 * RoadNetwork.cpp
 *
 *  Created on: Mar 11, 2012
 *      Author: martin
 */

#include "RoadNetwork.h"
#include <boost/range/iterator_range.hpp>
#include <boost/range/adaptors.hpp>

namespace roads
{

RoadNetwork::~RoadNetwork()
{
}

void RoadNetwork::add_edge(RoadEdgeWithNodes const& e)
{
    std::unique_ptr<RoadNetworkNode> start(new RoadNetworkNode());
    start->position = e.start_node.position;
    start->nd_id = e.start_node.id;
    std::unique_ptr<RoadNetworkNode> end(new RoadNetworkNode());
    end->position = e.end_node.position;
    end->nd_id = e.end_node.id;

    nodes_to_edges.insert(std::make_pair(std::make_pair(start.get(), end.get()), e));
    edge_starts.insert(std::make_pair(e.start_node.id, start.get()));
    edge_ends.insert(std::make_pair(e.end_node.id, end.get()));

    start->neighbours.push_back(std::make_pair(e.cost, end.get()));

    for (auto it = edge_starts.lower_bound(e.end_node.id); it != edge_starts.upper_bound(e.end_node.id); ++it)
    {
        end->neighbours.push_back(std::make_pair(0, it->second));
    }
    for (auto it = edge_ends.lower_bound(e.start_node.id); it != edge_ends.upper_bound(e.start_node.id); ++it)
    {
        it->second->neighbours.push_back(std::make_pair(0, start.get()));
    }
    nodes.push_back(std::move(start));
    nodes.push_back(std::move(end));
}

RoadNetworkNode* extract_node(std::pair<int64_t, RoadNetworkNode*> const& p)
{
    return p.second;
}

PathFindContext RoadNetwork::get_path_find_context(int64_t start_id, int64_t end_id)
{
    return PathFindContext(
               boost::make_iterator_range(edge_starts.lower_bound(start_id), edge_starts.upper_bound(start_id))
               | boost::adaptors::transformed(extract_node),
               boost::make_iterator_range(edge_ends.lower_bound(end_id), edge_ends.upper_bound(end_id))
               | boost::adaptors::transformed(extract_node)
           );
}

} /* namespace roads */
