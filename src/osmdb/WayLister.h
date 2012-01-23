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

/**
 * \class WayLister
 * Responsible for retrieving ways for the purpose of filtering member nodes.
 */
class WayLister
{
public:
    /**
     * Constructor
     * @param db underlying OsmDatabase
     * @param attributes ways containing these attributes will be fetchet
     * @param fetch_size amount of rows to fetch in one chunk
     */
    WayLister(OsmDatabase& db, std::multimap<std::string, std::string> const& attributes, unsigned int fetch_size = 100000);
    /**
     *
     * @return current buffer of fetched ways in the format used by wayred::WayNodeFilter
     */
    std::map<osm::Way, std::multimap<osm::Node, osm::Way, osm::LtByID>, osm::LtByID> const& get_current_connected_ways() const;
    /**
     * Fetch next chunk of data from database.
     */
    void next();
    /**
     * Return to beginning
     */
    void reset();
    /**
     * End retrieval and close underlying connection (no need to call this if you do not need to close cursor
     * before this object gets destroyed.
     * @return
     */
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
