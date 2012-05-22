/*
 * AStarPathFinding.h
 *
 *  Created on: May 8, 2012
 *      Author: martin
 */

#ifndef ASTARPATHFINDING_H_
#define ASTARPATHFINDING_H_

#include "AStar.h"
#include "../util/range.h"
#include "PathFindingAlgorithm.h"
#include <boost/range/algorithm.hpp>

namespace pathfind
{

template<typename Node, typename Cost, typename NeighFunc, typename HeuristicEstimate, typename CostCompare = std::less<Cost>, typename ClosedSet = std::unordered_set<Node>, typename OpenSet = OrderedCostNodeSet<Node, Cost, CostCompare>, typename CostMap = std::unordered_map<Node, Cost>, typename NodeMap = std::unordered_map<Node, Node> >
class AStarPathFinding : public PathFindingAlgorithm<Node>
{
private:
    NeighFunc get_neighbours;
    HeuristicEstimate distance_estimate;
    CostCompare cost_less;
    int stepcount;

    class FixedHeuristicEstimate
    {
    private:
        AStarPathFinding* tptr;
        std::vector<Node> const& end;
    public:
        FixedHeuristicEstimate(AStarPathFinding* tptr, std::vector<Node> const& end):
            tptr(tptr),
            end(end)
        {
        }
        Cost operator()(Node const& n) const
        {
            return tptr->distance_estimate(n, end);
        }
    };

    typedef AStar < Node,
            Cost,
            NeighFunc,
            FixedHeuristicEstimate,
            CostCompare,
            ClosedSet,
            OpenSet,
            CostMap,
            NodeMap > AStarType;

    std::vector<Node> reconstruct_path(Node const& last, AStarType& ast)
    {
        std::vector<Node> ret;
        auto nd = boost::make_optional(last);
        while (nd)
        {
            ret.push_back(*nd);
            nd = ast.get_previous(*nd);
        }
        std::vector<Node> ret2;
        ret2.reserve(ret.size());
        for (auto it = ret.rbegin(); it != ret.rend(); ++it)
            ret2.push_back(*it);
        return ret2;
    }
public:
    AStarPathFinding(int stepcount = 1, NeighFunc get_neighbours = NeighFunc(), HeuristicEstimate distance_estimate = HeuristicEstimate(), CostCompare cost_less = CostCompare()):
        get_neighbours(get_neighbours),
        distance_estimate(distance_estimate),
        cost_less(cost_less),
        stepcount(stepcount)
    {
    }

    std::vector<Node> find_path(std::vector<Node> const& start, std::vector<Node> const& end)
    {
        AStarType ast(get_neighbours,
                      FixedHeuristicEstimate(this, end),
                      cost_less);
        for (auto it = start.begin(); it != start.end(); ++it)
        {
            ast.add_start(*it, 0.0);
        }

        while (util::any(end | boost::adaptors::filtered([&ast](Node const & nd)
    {
        return !ast.is_closed(nd);
        })))
        {
            for (int i = 0; i < stepcount; ++i)
            {
                ast.step();
            }
        }

        auto nd = end.front();
        auto min = ast.get_cost(nd);
        for (auto it = end.begin(); it != end.end(); ++it)
        {
            auto v = ast.get_cost(*it);
            if (cost_less(v, min))
            {
                nd = *it;
                min = v;
            }
        }

        return reconstruct_path(nd, ast);
    }

    virtual ~AStarPathFinding()
    {
    }
};

template<typename Node, typename Cost, typename NeighFunc, typename HeuristicEstimate, typename CostCompare = std::less<Cost>, typename ClosedSet = std::unordered_set<Node>, typename OpenSet = OrderedCostNodeSet<Node, Cost, CostCompare>, typename CostMap = std::unordered_map<Node, Cost>, typename NodeMap = std::unordered_map<Node, Node> >
std::shared_ptr<AStarPathFinding<Node, Cost, NeighFunc, HeuristicEstimate, CostCompare, ClosedSet, OpenSet, CostMap, NodeMap> > get_astar(int count, NeighFunc get_neighbours = NeighFunc(), HeuristicEstimate cost_estimate = HeuristicEstimate(), CostCompare cost_less = CostCompare())
{
    return std::make_shared<AStarPathFinding<Node, Cost, NeighFunc, HeuristicEstimate, CostCompare, ClosedSet, OpenSet, CostMap, NodeMap> >(count, get_neighbours, cost_estimate, cost_less);
}

} /* namespace pathfinding */
#endif /* ASTARPATHFINDING_H_ */
