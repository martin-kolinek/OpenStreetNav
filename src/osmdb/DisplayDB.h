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
    std::vector<geo::Edge> const& get_edges();
    std::vector<geo::Point> const& get_points();
    OsmDatabase& get_db();
    void set_bounds(geo::Point const& topleft, geo::Point const& bottomright, int zoom);
    std::vector<std::unique_ptr<osm::Element> > get_selected(geo::Point const& topleft, geo::Point const& bottomright, int zoom);
    void set_to_show(std::string const& key, std::string const& value, int minzoom, int maxzoom);
private:
    osmdb::OsmDatabase& db;
    std::vector<geo::Point> points;
    std::vector<geo::Edge> edges;
    psql::Statement<psql::BindTypes<int, double, double, double, double>, psql::RetTypes<double, double, double, double> > edge_st;
    psql::Statement<psql::BindTypes<int, double, double, double, double>, psql::RetTypes<double, double> > node_st;
    psql::Statement<psql::BindTypes<std::string, std::string, int>, psql::RetTypes<> > to_show_ins;
    psql::Statement<psql::BindTypes<int, double, double, double, double>, psql::RetTypes<int64_t, std::string, std::string> > way_desc;
    psql::Statement<psql::BindTypes<int, double, double, double, double>, psql::RetTypes<int64_t, std::string, std::string> > node_desc;
};

} /* namespace display */
#endif /* DISPLAYDB_H_ */
