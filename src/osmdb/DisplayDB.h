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
#include "ToShowSelectCollection.h"

namespace osmdb
{

class DisplayDB
{
public:
    DisplayDB(OsmDatabase& db, std::string const& path_base, int min_zoom, int max_zoom);
    virtual ~DisplayDB();
    std::vector<geo::Edge> const& get_edges();
    std::vector<geo::Point> const& get_points();
    OsmDatabase& get_db();
    void set_bounds(geo::Point const& topleft, geo::Point const& bottomright, int zoom);
    std::vector<std::unique_ptr<osm::Element> > get_selected(geo::Point const& topleft, geo::Point const& bottomright, int zoom);
    double center_lat();
    double center_lon();
private:
    osmdb::OsmDatabase& db;
    std::vector<geo::Point> points;
    std::vector<geo::Edge> edges;
    double clat;
    double clon;
    int minz, maxz;
    ToShowSelectCollection coll;
    psql::Statement<psql::BindTypes<>, psql::RetTypes<double, double, double, double> > get_bounds;
};

} /* namespace display */
#endif /* DISPLAYDB_H_ */
