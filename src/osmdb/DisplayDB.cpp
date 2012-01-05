/*
 * DisplayDB.cpp
 *
 *  Created on: Nov 12, 2011
 *      Author: martin
 */

#include "DisplayDB.h"
#include "../sqllib/sqllib.h"
#include "ToShowEdgesSelector.h"

namespace osmdb
{

DisplayDB::DisplayDB(OsmDatabase& db, std::string const& path_base, int min_zoom, int max_zoom)
    : db(db),
      pdb(db),
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

std::vector<std::unique_ptr<display::DisplayElement> > const& DisplayDB::get_display_elements()
{
    return display_elements;
}

void DisplayDB::set_bounds(const geo::Point& p1, const geo::Point& p2, int zoom)
{
    display_elements.clear();
    double left = std::min(p1.lon, p2.lon);
    double right = std::max(p1.lon, p2.lon);
    double lower = std::min(p1.lat, p2.lat);
    double higher = std::max(p1.lat, p2.lat);
    auto& stmt = coll.get_edges_for_zoom(zoom);
    std::vector<std::unique_ptr<display::DisplayElement> > edges = std::move(ToShowEdgesSelector::get_edges(stmt, left, lower, right, higher));
    for (unsigned int i = 0; i < edges.size(); ++i)
    {
        display_elements.push_back(std::unique_ptr<display::DisplayElement>(std::move(edges[i])));
    }
}

std::vector<std::unique_ptr<osm::Element> > DisplayDB::get_selected(const geo::Point& p1, const geo::Point& p2, int zoom)
{
    double left = std::min(p1.lon, p2.lon);
    double right = std::max(p1.lon, p2.lon);
    double lower = std::min(p1.lat, p2.lat);
    double higher = std::max(p1.lat, p2.lat);

    std::vector<std::unique_ptr<osm::Element> > ret;
    auto& stmt = coll.get_select_edges(zoom);
    stmt.execute(left, lower, right, higher);
    for (int i = 0; i < stmt.row_count(); ++i)
    {
        int64_t id;
        double r, g, b, a, t;
        int attr;
        std::tie(id, r, g, b, a, t, attr) = stmt.get_row(i);
        ret.push_back(std::unique_ptr<osm::Element>(new osm::Way(id)));
    }

    for (int i = 0; i < ret.size(); ++i)
    {
        ret[i]->fill(pdb);
    }

    return ret;
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
