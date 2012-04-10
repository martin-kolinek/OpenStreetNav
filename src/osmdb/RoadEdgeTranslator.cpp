/*
 * RoadEdgeTranslator.cpp
 *
 *  Created on: Apr 11, 2012
 *      Author: martin
 */

#include "RoadEdgeTranslator.h"

namespace osmdb
{

RoadEdgeTranslator::RoadEdgeTranslator(OsmDatabase& db):
    prop(db)
{
}

std::vector<std::shared_ptr<display::Descriptible> > RoadEdgeTranslator::translate(std::vector<osm::Edge> const& v)
{
    std::vector<std::shared_ptr<display::Descriptible> > ret;

    for (auto it = v.begin(); it != v.end(); ++it)
    {
        ret.push_back(std::shared_ptr<display::Descriptible>(new display::DescriptibleRoadEdge(
                          prop.get_properties(it->get_way().id, it->get_start_seq_no(), it->get_end_seq_no()))));
    }

    return ret;
}

RoadEdgeTranslator::~RoadEdgeTranslator()
{
}

} /* namespace osmdb */
