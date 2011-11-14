/*
 * Edge.h
 *
 *  Created on: Nov 12, 2011
 *      Author: martin
 */

#ifndef EDGE_H_
#define EDGE_H_

#include <stdint.h>

namespace osm
{

class Edge
{
public:
    Edge(int64_t start_node = 0, int64_t end_node = 0, int64_t way_id = 0);
    virtual ~Edge();
    int64_t start_node;
    int64_t end_node;
    int64_t way_id;
};

} /* namespace display */
#endif /* EDGE_H_ */
