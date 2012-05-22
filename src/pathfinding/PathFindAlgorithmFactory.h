/*
 * PathFindAlgorithmFactory.h
 *
 *  Created on: May 21, 2012
 *      Author: martin
 */

#ifndef PATHFINDALGORITHMFACTORY_H_
#define PATHFINDALGORITHMFACTORY_H_

#include <memory>
#include "PathFindingAlgorithm.h"
#include "../roads/roads.h"

namespace pathfind
{

class PathFindAlgorithmFactory
{
public:
    static std::shared_ptr<PathFindingAlgorithm<roads::RoadNetworkNode const*>> get_astar_algorithm(int step);
};

} /* namespace pathfind */
#endif /* PATHFINDALGORITHMFACTORY_H_ */
