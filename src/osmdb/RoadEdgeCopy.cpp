/*
 * RoadEdgeCopy.cpp
 *
 *  Created on: Mar 13, 2012
 *      Author: martin
 */

#include "RoadEdgeCopy.h"
#include "../sqllib/sqllib.h"

namespace osmdb
{

RoadEdgeCopy::RoadEdgeCopy(OsmDatabase& db):
    db(db),
    st(sqllib::get_copy_road_network(db.get_db()))
{
}

void RoadEdgeCopy::start_copy()
{
    st.execute();
}

void RoadEdgeCopy::copy_edge(const roads::RoadEdgeWithNodes& re)
{
    st.copy_data(re.way.id, re.start_node.id, re.end_node.id, re.start_seq_no, re.end_seq_no, re.start_node.position.lon, re.start_node.position.lat, re.end_node.position.lon, re.end_node.position.lat, re.forward, re.cost);
}

void RoadEdgeCopy::end_copy()
{
    st.end_copy();
}

RoadEdgeCopy::~RoadEdgeCopy()
{
}

} /* namespace osmdb */
