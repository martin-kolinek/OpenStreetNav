/*
 * AStarAreaFinding.h
 *
 *  Created on: May 23, 2012
 *      Author: martin
 */

#ifndef ASTARAREAFINDING_H_
#define ASTARAREAFINDING_H_

#include "AreaAlgorithm.h"
#include <iostream>
#include <memory>
#include "AStar.h"

namespace pathfind
{

template<typename Node, typename Cost, typename NeighFunc, typename CostCompare = std::less<Cost>, typename ClosedSet = std::unordered_set<Node>, typename OpenSet = OrderedCostNodeSet<Node, Cost, CostCompare>, typename CostMap = std::unordered_map<Node, Cost>, typename NodeMap = std::unordered_map<Node, Node> >
class AStarAreaFinding : public AreaAlgorithm<Node, Cost>
{
private:
    NeighFunc get_neighbours;
    CostCompare cost_less;
    int stepcount;
    class Zero
    {
    public:
        Cost operator()(Node const&)
        {
            return Cost(0);
        }
    };
    typedef AStar < Node,
            Cost,
            NeighFunc,
            Zero,
            CostCompare,
            ClosedSet,
            OpenSet,
            CostMap,
            NodeMap > AStarType;
public:
    AStarAreaFinding(int stepcount = 1, NeighFunc get_neighbours = NeighFunc(), CostCompare cost_less = CostCompare()):
        get_neighbours(get_neighbours),
        cost_less(cost_less),
        stepcount(stepcount)
    {

    }
    virtual ~AStarAreaFinding()
    {

    }
    std::vector<Node> find_area(std::vector<Node> const& start, Cost const& max_cost)
    {
        AStarType ast(get_neighbours,
                      Zero(),
                      cost_less);
        for (auto it = start.begin(); it != start.end(); ++it)
            ast.add_start(*it, Cost(0));
        while (!ast.done() && cost_less(ast.get_next_cost(), max_cost))
        {
            for (int i = 0; i < stepcount; ++i)
            {
                ast.step();
            }
        }

        auto rng = ast.get_closed();
        std::vector<Node> ret;
        for (auto it = rng.begin(); it != rng.end(); ++it)
        {
            if (cost_less(ast.get_cost(*it), max_cost))
            {
                ret.push_back(*it);
            }
        }

        return ret;
    }
};

template<typename Node, typename Cost, typename NeighFunc, typename CostCompare = std::less<Cost>, typename ClosedSet = std::unordered_set<Node>, typename OpenSet = OrderedCostNodeSet<Node, Cost, CostCompare>, typename CostMap = std::unordered_map<Node, Cost>, typename NodeMap = std::unordered_map<Node, Node> >
std::shared_ptr<AStarAreaFinding<Node, Cost, NeighFunc, CostCompare, ClosedSet, OpenSet, CostMap, NodeMap> > get_astar_area(int count, NeighFunc get_neighbours = NeighFunc(), CostCompare cost_less = CostCompare())
{
    return std::make_shared<AStarAreaFinding<Node, Cost, NeighFunc, CostCompare, ClosedSet, OpenSet, CostMap, NodeMap> >(count, get_neighbours, cost_less);
}

} /* namespace pathfind */
#endif /* ASTARAREAFINDING_H_ */
