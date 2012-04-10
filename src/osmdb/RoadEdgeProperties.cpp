/*
 * RoadEdgeProperties.cpp
 *
 *  Created on: Apr 11, 2012
 *      Author: martin
 */

#include "RoadEdgeProperties.h"
#include "PropertiesSelectionException.h"
#include "../sqllib/sqllib.h"
#include "../displayer/DescriptibleRoadEdge.h"

namespace osmdb
{

RoadEdgeProperties::RoadEdgeProperties(OsmDatabase odb):
    stmt(sqllib::get_select_road_edge_properties(odb.get_db(), true, "road_edge_prop_stmt"))
{
}

display::DescriptibleRoadEdge RoadEdgeProperties::get_properties(int64_t way_id, int start_seq_no, int end_seq_no)
{
    stmt.execute(way_id, start_seq_no, end_seq_no);
    if (stmt.row_count() < 1)
        throw PropertiesSelectionException(util::concatenate(" ", "Cannot find properties of road edge", way_id, start_seq_no, end_seq_no));
    int64_t wid, snid, enid;
    int ssq, esq, dir;
    double cost;
    std::tie(wid, ssq, snid, esq, enid, dir, cost) = stmt.get_row(0);
    return display::DescriptibleRoadEdge((display::DescRoadEdgeDir)dir, wid, ssq, snid, esq, enid, cost);
}

RoadEdgeProperties::~RoadEdgeProperties()
{
}

} /* namespace osmdb */
