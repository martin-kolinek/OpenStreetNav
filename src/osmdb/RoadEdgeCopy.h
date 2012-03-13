/*
 * RoadEdgeCopy.h
 *
 *  Created on: Mar 13, 2012
 *      Author: martin
 */

#ifndef ROADEDGECOPY_H_
#define ROADEDGECOPY_H_

#include "OsmDatabase.h"
#include "../roads/roads.h"

namespace osmdb
{

class RoadEdgeCopy
{
public:
    RoadEdgeCopy(OsmDatabase& db);
    void start_copy();
    void copy_edge(roads::RoadEdge const& re);
    void end_copy();
    virtual ~RoadEdgeCopy();
private:
    OsmDatabase& db;
    psql::Statement<psql::BindTypes<>, psql::RetTypes<>, psql::CopyTypes<int64_t, int, bool, double> > st;
};

} /* namespace osmdb */
#endif /* ROADEDGECOPY_H_ */
