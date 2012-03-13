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

void RoadEdgeCopy::copy_edge(const roads::RoadEdge& re)
{
    st.copy_data(re.way_id, re.seq_no, re.forward, re.cost);
}

void RoadEdgeCopy::end_copy()
{
    st.end_copy();
}

RoadEdgeCopy::~RoadEdgeCopy()
{
}

} /* namespace osmdb */
