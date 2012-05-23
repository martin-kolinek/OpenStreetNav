/*
 * AreaAlgorithm.h
 *
 *  Created on: May 23, 2012
 *      Author: martin
 */

#ifndef AREAALGORITHM_H_
#define AREAALGORITHM_H_

#include <vector>

namespace pathfind
{

template<typename Node, typename Cost>
class AreaAlgorithm
{
public:
    virtual ~AreaAlgorithm()
    {
    }
    virtual std::vector<Node> find_area(std::vector<Node> const& start, Cost const& max_cost) = 0;
};

} /* namespace pathfind */
#endif /* AREAALGORITHM_H_ */
