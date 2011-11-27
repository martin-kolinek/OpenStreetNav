/*
 * DisplayDB.h
 *
 *  Created on: Nov 12, 2011
 *      Author: martin
 */

#ifndef DISPLAYDB_H_
#define DISPLAYDB_H_

#include "../osmdb/osmdb.h"
#include "../geoelements/geoelements.h"
#include "../elements/Node.h"
#include <unordered_map>
#include <unordered_set>
#include <vector>

namespace osmdb
{

class DisplayDB
{
public:
    DisplayDB(OsmDatabase& db);
    virtual ~DisplayDB();
    std::vector<osm::Edge> const& get_edges();
    std::unordered_map<int64_t, osm::Node> const& get_nodes();
    std::unordered_set<int64_t> const& get_free_nodes();
    OsmDatabase& get_db();
    void set_to_show(std::string const& key, std::string const& val, int minzoom, int maxzoom);
    void set_bounds(geo::Point const& topleft, geo::Point const& bottomright, int zoom);
private:
    osmdb::OsmDatabase& db;
    std::unordered_map<int64_t, osm::Node> nodes;
    std::vector<osm::Edge> edges;
    std::unordered_set<int64_t> free_nodes;
};

} /* namespace display */
#endif /* DISPLAYDB_H_ */
