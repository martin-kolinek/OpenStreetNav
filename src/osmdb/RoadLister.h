/*
 * RoadLister.h
 *
 *  Created on: Mar 21, 2012
 *      Author: martin
 */

#ifndef ROADLISTER_H_
#define ROADLISTER_H_

#include "OsmDatabase.h"
#include "../roads/roads.h"

namespace osmdb
{

class RoadLister
{
public:
    RoadLister(OsmDatabase& db);
    virtual ~RoadLister();
    std::vector<roads::RoadEdgeWithNodes> get_edges();
    void fill_road_network(roads::RoadNetwork& net);
private:
    OsmDatabase& db;
};

} /* namespace osmdb */
#endif /* ROADLISTER_H_ */
