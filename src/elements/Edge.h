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
    Edge(Node const& start_node, int start_seq_no, Node const& end_node, int end_seq_no, osm::Way const& way);
    int& get_start_seq_no()
    {
        return start_seq_no;
    }
    int const& get_start_seq_no() const
    {
        return start_seq_no;
    }
    int& get_end_seq_no()
    {
        return end_seq_no;
    }
    int const& get_end_seq_no() const
    {
        return end_seq_no;
    }
    osm::Way const& get_way() const
    {
        return way;
    }
    osm::Way& get_way()
    {
        return way;
    }
    Node& get_start_node()
    {
        return start_node;
    }
    Node const& get_start_node() const
    {
        return start_node;
    }
    Node& get_end_node()
    {
        return end_node;
    }
    Node const& get_end_node() const
    {
        return end_node;
    }
    virtual ~Edge();
private:
    Node start_node;
    int start_seq_no;
    Node end_node;
    int end_seq_no;
    Way way;
};

} /* namespace display */
#endif /* EDGE_H_ */
