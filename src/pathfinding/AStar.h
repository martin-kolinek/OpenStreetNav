/*
 * AStar.h
 *
 *  Created on: Jan 26, 2012
 *      Author: martin
 */

#ifndef ASTAR_H_
#define ASTAR_H_

#include "OrderedCostNodeSet.h"
#include <set>
#include <unordered_map>
#include <unordered_set>
#include <boost/optional.hpp>
#include <boost/range.hpp>
#include <boost/range/any_range.hpp>

namespace pathfind
{

template<typename Node, typename Cost, typename NeighFunc, typename HeuristicEstimate, typename CostCompare = std::less<Cost>, typename ClosedSet = std::unordered_set<Node>, typename OpenSet = OrderedCostNodeSet<Node, Cost, CostCompare>, typename CostMap = std::unordered_map<Node, Cost>, typename NodeMap = std::unordered_map<Node, Node> >
class AStar
{
private:
    ClosedSet closed;
    OpenSet open;
    CostMap costs;
    NodeMap came_from;
    NeighFunc get_neighbours;
    HeuristicEstimate cost_estimate;
    CostCompare cost_less;
public:
    AStar(NeighFunc get_neighbours = NeighFunc(), HeuristicEstimate cost_estimate = HeuristicEstimate(), CostCompare cost_less = CostCompare()):
        get_neighbours(get_neighbours),
        cost_estimate(cost_estimate),
        cost_less(cost_less)
    {
    }

    void add_start(Node const& nd, Cost initial_cost)
    {
        open.insert(initial_cost + cost_estimate(nd), nd);
        costs[nd] = initial_cost;
    }

    void step()
    {
        if (open.empty())
            return;
        Node n = open.top();
        open.pop();
        closed.insert(n);
        auto neighs = get_neighbours(n);
        for (auto it = neighs.begin(); it != neighs.end(); ++it)
        {
            Node n2 = it->second;
            Cost c = it->first;
            if (is_closed(n2))
                continue;
            Cost new_cost = c + costs[n];
            auto open_pos = open.find(n2);
            if (open_pos == open.end() || cost_less(new_cost, costs[n2]))
            {
                open.insert(new_cost + cost_estimate(n2), n2);
                costs[n2] = new_cost;
                came_from[n2] = n;
            }
        }
    }

    Cost get_cost(Node const& nd)
    {
        return costs[nd];
    }

    bool is_closed(Node const& nd)
    {
        return closed.find(nd) != closed.end();
    }

    Cost get_next_cost()
    {
        return open.top_cost();
    }

    boost::any_range<Node, boost::forward_traversal_tag, Node, size_t> get_closed()
    {
        return closed;
    }

    bool done()
    {
        return open.empty();
    }

    boost::optional<Node> get_previous(Node const& nd)
    {
        auto it = came_from.find(nd);
        if (it != came_from.end())
        {
            return boost::make_optional(it->second);
        }
        return boost::optional<Node>();
    }

private:
    std::vector<Node> reconstruct_path(Node const& last, NodeMap const& ndmap)
    {
        std::vector<Node> ret;
        ret.push_back(last);
        auto it = ndmap.find(last);
        while (it != ndmap.end())
        {
            ret.push_back(it->second);
            it = ndmap.find(it->second);
        }
        return ret;
    }

};

}

#endif /* ASTAR_H_ */
