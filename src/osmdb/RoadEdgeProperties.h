/*
 * RoadEdgeProperties.h
 *
 *  Created on: Apr 11, 2012
 *      Author: martin
 */

#ifndef ROADEDGEPROPERTIES_H_
#define ROADEDGEPROPERTIES_H_

#include "OsmDatabase.h"
#include "../displayer/DescriptibleRoadEdge.h"

namespace osmdb
{

class RoadEdgeProperties
{
public:
    RoadEdgeProperties(OsmDatabase odb);
    display::DescriptibleRoadEdge get_properties(int64_t way_id, int start_seq_no, int end_seq_no);
    virtual ~RoadEdgeProperties();
private:
    psql::Statement<psql::BindTypes<int64_t, int, int>, psql::RetTypes<int64_t, int, int64_t, int, int64_t, int, double> > stmt;
};

} /* namespace osmdb */
#endif /* ROADEDGEPROPERTIES_H_ */
