/*
 * WayNodeFiltering.h
 *
 *  Created on: Jan 13, 2012
 *      Author: martin
 */

#ifndef WAYNODEFILTER_H_
#define WAYNODEFILTER_H_

#include <vector>
#include "../elements/osmelements.h"
#include <map>

namespace wayred
{

class WayNodeFilter
{
public:
    WayNodeFilter();
    virtual ~WayNodeFilter();
    std::vector<osm::Way> reduce_ways(std::map<osm::Way, std::multimap<osm::Node, osm::Way, osm::LtByID>, osm::LtByID> const& old_ways);
    osm::Way process_way(osm::Way const& w, std::multimap<osm::Node, osm::Way, osm::LtByID> const& ndmap);
    void add_important(std::string const& key, std::string const& val);
private:
    std::set<std::pair<std::string, std::string> > important;
    bool has_important_ways(osm::Node const& n, std::multimap<osm::Node, osm::Way, osm::LtByID> const& ndmap);
};

} /* namespace wayred */
#endif /* WAYNODEFILTER_H_ */
