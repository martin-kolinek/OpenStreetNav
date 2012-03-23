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

namespace pathfind
{

template<typename Node, typename NodeMap>
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

template < typename Node, typename Cost, typename NeighFunc, typename HeuristicEstimate, typename IsEndFunc, typename CostCompare = std::less<Cost>, typename ClosedSet = std::unordered_set<Node>, typename OpenSet = OrderedCostNodeSet<Node, Cost, CostCompare>, typename CostMap = std::unordered_map<Node, Cost>, typename NodeMap = std::unordered_map<Node, Node> >
std::vector<Node> find_path(Node const& start, Cost initial_cost, NeighFunc get_neighbours = NeighFunc(), HeuristicEstimate cost_estimate = HeuristicEstimate(), IsEndFunc is_end = IsEndFunc(), CostCompare cost_less = CostCompare())
{

    ClosedSet closed;
    OpenSet open;
    open.insert(initial_cost + cost_estimate(start), start);
    CostMap costs;
    costs[start] = initial_cost;
    NodeMap came_from;
    while (!open.empty())
    {
        Node n = open.top();
        if (is_end(n))
        {
            auto vect = reconstruct_path(n, came_from);
            std::reverse(vect.begin(), vect.end());
            return vect;
        }
        open.pop();
        closed.insert(n);
        auto neighs = get_neighbours(n);
        for (auto it = neighs.begin(); it != neighs.end(); ++it)
        {
            Node n2 = it->second;
            Cost c = it->first;
            if (closed.find(n2) != closed.end())
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
    return std::vector<Node>();
}

}

#endif /* ASTAR_H_ */
