/*
 * RoadNetworkCreator.h
 *
 *  Created on: Mar 3, 2012
 *      Author: martin
 */

#ifndef ROADNETWORKCREATOR_H_
#define ROADNETWORKCREATOR_H_

#include <map>
#include <string>
#include "OsmDatabase.h"

namespace osmdb
{

class RoadNetworkCreator
{
public:
    RoadNetworkCreator(OsmDatabase& full, OsmDatabase& reduced, OsmDatabase& destination, std::multimap<std::string, std::string> const& attributes);
    void create_road_network_table();
    void copy_road_network_data();
    virtual ~RoadNetworkCreator();
private:
    OsmDatabase& full;
    OsmDatabase& reduced;
    OsmDatabase& destination;
    std::multimap<std::string, std::string> attributes;
};

} /* namespace osmdb */
#endif /* ROADNETWORKCREATOR_H_ */
