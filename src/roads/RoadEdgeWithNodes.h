/*
 * RoadEdgeWithNodes.h
 *
 *  Created on: Mar 11, 2012
 *      Author: martin
 */

#ifndef ROADEDGEWITHNODES_H_
#define ROADEDGEWITHNODES_H_

#include "RoadEdge.h"

namespace roads
{

class RoadEdgeWithNodes : public RoadEdge
{
public:
    RoadEdgeWithNodes(int64_t way_id, int seq_no, bool forward, double cost, int64_t start_node_id, int64_t end_node_id);
    virtual ~RoadEdgeWithNodes();
    int64_t start_node_id, end_node_id;
};

} /* namespace roads */
#endif /* ROADEDGEWITHNODES_H_ */
