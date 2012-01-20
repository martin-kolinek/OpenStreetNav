/*
 * WayLister.h
 *
 *  Created on: Jan 12, 2012
 *      Author: martin
 */

#ifndef WAYLISTER_H_
#define WAYLISTER_H_

#include "OsmDatabase.h"
#include <string>
#include "../elements/osmelements.h"
#include "../psql/psql.h"
#include <vector>
#include <map>

namespace osmdb
{

class WayLister
{
public:
    WayLister(OsmDatabase& db, std::multimap<std::string, std::string> const& attributes, unsigned int fetch_size = 100000);
    std::map<osm::Way, std::multimap<osm::Node, osm::Way, osm::LtByID>, osm::LtByID> const& get_current_connected_ways() const;
    void next();
    void reset();
    bool end();
    virtual ~WayLister();
private:
    OsmDatabase& db;
    psql::Cursor<psql::BindTypes<>, psql::RetTypes<int64_t, int64_t, double, double, int64_t, std::string, std::string, int> > get_way_descr;
    std::vector<std::tuple<int64_t, int64_t, double, double, int64_t, std::string, std::string, int> > rest;
    std::map<osm::Way, std::multimap<osm::Node, osm::Way, osm::LtByID>, osm::LtByID> current_connected_ways;
    boost::property_tree::ptree get_entries(std::multimap<std::string, std::string> const& attributes);
    bool done;
    unsigned int fetch_size;
};

} /* namespace osmdb */
#endif /* WAYLISTER_H_ */
