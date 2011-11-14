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
#include <vector>

namespace display
{

class DisplayDB
{
public:
    DisplayDB(std::string const& filename);
    virtual ~DisplayDB();
    std::vector<osm::Edge> const& get_edges();
    std::unordered_map<int64_t, osm::Node> get_nodes();
    std::vector<int64_t> const& get_free_nodes();
    void set_bounds(geo::Point const& topleft, geo::Point const& bottomright, int zoom);
private:
    osmdb::OsmDatabase db;
    std::unordered_map<int64_t, osm::Node> nodes;
    std::vector<osm::Edge> edges;
    std::vector<int64_t> free_nodes;
    sqlite::Statement copy_stmt1;
    sqlite::Statement copy_stmt2;
    sqlite::Statement clear_stmt1;
    sqlite::Statement clear_stmt2;
    sqlite::Statement select_stmt1;
    sqlite::Statement select_stmt2;
};

} /* namespace display */
#endif /* DISPLAYDB_H_ */
