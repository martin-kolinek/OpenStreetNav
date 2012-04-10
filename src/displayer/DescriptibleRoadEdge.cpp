/*
 * DescriptibleRoadEdge.cpp
 *
 *  Created on: Apr 11, 2012
 *      Author: martin
 */

#include "DescriptibleRoadEdge.h"
#include "../elements/osmelements.h"

namespace display
{

DescriptibleRoadEdge::DescriptibleRoadEdge(DescRoadEdgeDir direction, int64_t way_id, int start_seq_no, int64_t start_node_id, int end_seq_no, int64_t end_node_id, double cost):
    direction(direction),
    way_id(way_id),
    start_node_id(start_node_id),
    end_node_id(end_node_id),
    start_seq_no(start_seq_no),
    end_seq_no(end_seq_no),
    cost(cost)
{
}

std::string get_dir_desc(DescRoadEdgeDir dir)
{
    switch (dir)
    {
        case DescRoadEdgeDir::Both:
            return "both";
        case DescRoadEdgeDir::Forward:
            return "forward";
        case DescRoadEdgeDir::Backward:
            return "backward";
    }
    return "";
}

boost::property_tree::ptree DescriptibleRoadEdge::get_description() const
{
    boost::property_tree::ptree ret;
    boost::property_tree::ptree t;
    t.put("way", way_id);
    t.put("start node", start_node_id);
    t.put("end node", end_node_id);
    t.put("start seq", start_seq_no);
    t.put("end seq", end_seq_no);
    t.put("direction", get_dir_desc(direction));
    t.put("cost", cost);
    ret.put_child("edge", t);
    return ret;
}

std::vector<std::shared_ptr<osm::HashElementContainer> > DescriptibleRoadEdge::get_highlighted() const
{
    std::vector<std::pair<int, int> > regs {std::make_pair(start_seq_no, end_seq_no)};
    std::vector<std::shared_ptr<osm::HashElementContainer> > ret;
    ret.push_back(std::shared_ptr<osm::HashElementContainer>(new osm::WayRegion(osm::Way(way_id), regs)));
    return ret;
}

DescriptibleRoadEdge::~DescriptibleRoadEdge()
{
}

} /* namespace display */
