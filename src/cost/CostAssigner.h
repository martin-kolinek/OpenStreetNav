/*
 * CostAssigner.h
 *
 *  Created on: Mar 4, 2012
 *      Author: martin
 */

#ifndef COSTASSIGNER_H_
#define COSTASSIGNER_H_

#include <vector>
#include "../elements/osmelements.h"
#include "../roads/roads.h"

namespace cost
{

class CostAssigner
{
public:
    virtual ~CostAssigner();
    virtual std::vector<roads::RoadEdgeWithNodes> extract_edges(osm::Way const& reduced, osm::Way const& full) = 0;
};

} /* namespace cost */
#endif /* COSTASSIGNER_H_ */
