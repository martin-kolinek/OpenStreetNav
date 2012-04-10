/*
 * DescriptibleRoadEdge.h
 *
 *  Created on: Apr 11, 2012
 *      Author: martin
 */

#ifndef DESCRIPTIBLEROADEDGE_H_
#define DESCRIPTIBLEROADEDGE_H_

#include "Descriptible.h"

namespace display
{

enum class DescRoadEdgeDir
{
    Both = 0,
    Forward = 1,
    Backward = 2
};

std::string get_dir_desc(DescRoadEdgeDir dir);

class DescriptibleRoadEdge : public Descriptible
{
public:
    DescriptibleRoadEdge(DescRoadEdgeDir direction, int64_t way_id, int start_seq_no, int64_t start_node_id, int end_seq_no, int64_t end_node_id, double cost);
    boost::property_tree::ptree get_description() const;
    std::vector<std::shared_ptr<osm::HashElementContainer> > get_highlighted() const;
    virtual ~DescriptibleRoadEdge();
private:
    DescRoadEdgeDir direction;
    int64_t way_id, start_node_id, end_node_id;
    int start_seq_no, end_seq_no;
    double cost;
};

} /* namespace display */
#endif /* DESCRIPTIBLEROADEDGE_H_ */
