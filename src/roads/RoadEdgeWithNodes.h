/*
 * RoadEdgeWithNodes.h
 *
 *  Created on: Mar 11, 2012
 *      Author: martin
 */

#ifndef ROADEDGEWITHNODES_H_
#define ROADEDGEWITHNODES_H_

#include "../elements/osmelements.h"
#include "RoadEdge.h"

namespace roads
{

class RoadEdgeWithNodes : public RoadEdge
{
public:
    RoadEdgeWithNodes(int64_t way_id, int start_seq_no, int end_seq_no, bool forward, double cost, osm::Node start_node, osm::Node end_node);
    virtual ~RoadEdgeWithNodes();
    osm::Node& get_start_node();
    osm::Node& get_end_node();
    osm::Node const& get_start_node() const;
    osm::Node const& get_end_node() const;
    osm::Node start_node, end_node;
};

} /* namespace roads */
#endif /* ROADEDGEWITHNODES_H_ */
