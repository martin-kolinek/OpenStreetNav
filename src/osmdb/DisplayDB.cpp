/*
 * DisplayDB.cpp
 *
 *  Created on: Nov 12, 2011
 *      Author: martin
 */

#include "DisplayDB.h"
#include "../sqllib/sqllib.h"

namespace osmdb
{

DisplayDB::DisplayDB(OsmDatabase& db, std::string const& path_base, int min_zoom, int max_zoom)
    : db(db),
      minz(min_zoom),
      maxz(max_zoom),
      coll(path_base, db.get_db(), min_zoom, max_zoom),
      get_bounds(sqllib::get_select_bounds(db.get_db()))
{
    get_bounds.execute();
    if (get_bounds.row_count() == 0)
    {
        clat = clon = 0;
    }
    else
    {
        auto tpl = get_bounds.get_row(0);
        clon = (std::get<0>(tpl) + std::get<1>(tpl)) / 2;
        clat = (std::get<2>(tpl) + std::get<3>(tpl)) / 2;
    }
}

DisplayDB::~DisplayDB()
{
}

OsmDatabase& DisplayDB::get_db()
{
    return db;
}

const std::vector<geo::Edge> & DisplayDB::get_edges()
{
    return edges;
}

const std::vector<geo::Point> & DisplayDB::get_points()
{
    return points;
}

void DisplayDB::set_bounds(const geo::Point& p1, const geo::Point& p2, int zoom)
{
    edges.clear();
    points.clear();
    double left = std::min(p1.lon, p2.lon);
    double right = std::max(p1.lon, p2.lon);
    double lower = std::min(p1.lat, p2.lat);
    double higher = std::max(p1.lat, p2.lat);
    auto& stmt = coll.get_edges_for_zoom(zoom);
    stmt.execute(left, lower, right, higher);/*
    for (int i = 0; i < stmt.row_count(); ++i)
    {
        double a, b, c, d, red, green, blue, th;
        int64_t wayid;
        int arrow, polygon;
        std::tie(wayid, a, b, c, d, red, green, blue, th, arrow, polygon) = stmt.get_row(i);
        edges.push_back(geo::Edge(geo::Point(b, a), geo::Point(d, c)));
    }*/

}

std::vector<std::unique_ptr<osm::Element> > DisplayDB::get_selected(const geo::Point&, const geo::Point&, int)
{
    /*    double left = std::min(p1.lon, p2.lon);
        double right = std::max(p1.lon, p2.lon);
        double lower = std::min(p1.lat, p2.lat);
        double higher = std::max(p1.lat, p2.lat);
    */
    return std::vector<std::unique_ptr<osm::Element> >();
}

double DisplayDB::center_lat()
{
    return clat;
}

double DisplayDB::center_lon()
{
    return clon;
}

} /* namespace display */
