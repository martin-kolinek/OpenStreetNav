/*
 * DisplayDB.cpp
 *
 *  Created on: Nov 12, 2011
 *      Author: martin
 */

#include "DisplayDB.h"
#include "../sqllib/sqllib.h"
#include "ToShowEdgesSelector.h"
#include "../displayer/DisplayLine.h"
#include <boost/range/adaptors.hpp>

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

std::shared_ptr<display::DisplayElement> extract_disp_el(std::pair<osm::Edge, display::LineDisplayStyle> const& p)
{
    display::DisplayLine* ret = new display::DisplayLine(p.first.start_node.position, p.first.end_node.position,
            std::unique_ptr<display::LineDisplayStyle>(new display::LineDisplayStyle(p.second)));
    return std::shared_ptr<display::DisplayElement>(ret);
}

DisplayDB::element_range DisplayDB::get_display_elements()
{
    return display_elements | boost::adaptors::transformed(extract_disp_el);
}

osm::Edge extract_edge(std::pair<osm::Edge, display::LineDisplayStyle> const& p)
{
    return p.first;
}

DisplayDB::edge_range DisplayDB::get_edges()
{
    return display_elements | boost::adaptors::transformed(extract_edge);
}

void DisplayDB::set_bounds(const geo::Point& p1, const geo::Point& p2, int zoom)
{
    display_elements.clear();
    double left = std::min(p1.lon, p2.lon);
    double right = std::max(p1.lon, p2.lon);
    double lower = std::min(p1.lat, p2.lat);
    double higher = std::max(p1.lat, p2.lat);
    auto& stmt = coll.get_edges_for_zoom(zoom);
    display_elements = std::move(ToShowEdgesSelector::get_edges(stmt, left, lower, right, higher));
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
        int attr, p;
        std::tie(id, r, g, b, a, t, attr, p) = stmt.get_row(i);
        ret.push_back(std::unique_ptr<osm::Element>(new osm::Way(id)));
    }

    for (unsigned int i = 0; i < ret.size(); ++i)
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
