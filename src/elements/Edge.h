/*
 * Edge.h
 *
 *  Created on: Nov 12, 2011
 *      Author: martin
 */

#ifndef EDGE_H_
#define EDGE_H_

#include <stdint.h>
#include "Node.h"
#include "Way.h"

namespace osm
{

class Edge
{
public:
    Edge(Node const& start_node, Node const& end_node, osm::Way const& way);
    virtual ~Edge();
    Node start_node;
    Node end_node;
    Way way;
};

} /* namespace display */
#endif /* EDGE_H_ */
