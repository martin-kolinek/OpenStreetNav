/*
 * PathFindingAlgorithm.h
 *
 *  Created on: May 8, 2012
 *      Author: martin
 */

#ifndef PATHFINDINGALGORITHM_H_
#define PATHFINDINGALGORITHM_H_

#include <vector>

namespace pathfind
{

template<typename NodeType>
class PathFindingAlgorithm
{
public:
    virtual std::vector<NodeType> find_path(std::vector<NodeType> const& start, std::vector<NodeType> const& end) = 0;
    virtual ~PathFindingAlgorithm()
    {
    }
};

} /* namespace pathfinding */
#endif /* PATHFINDINGALGORITHM_H_ */
