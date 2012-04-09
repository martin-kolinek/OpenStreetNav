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
#include "../displayer/DescriptibleElement.h"
#include <boost/range/adaptors.hpp>

namespace osmdb
{

DisplayDB::DisplayDB(osmdb::OsmDatabase& db, std::vector<std::string> schemas, int offset, std::shared_ptr<EdgeTranslator> const& transl)
    : db(db),
      minz(offset),
      maxz(offset + schemas.size() - 1),
      coll(schemas, offset, db.get_db()),
      transl(transl),
      get_bounds(sqllib::get_select_bounds(db.get_db()))
{
    if (!this->transl)
        this->transl = std::shared_ptr<EdgeTranslator>(new EdgeTranslator());
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
    display::DisplayLine* ret = new display::DisplayLine(p.first,
            std::unique_ptr<display::LineDisplayStyle>(new display::LineDisplayStyle(p.second)));
    return std::shared_ptr<display::DisplayElement>(ret);
}

DisplayDB::element_range DisplayDB::get_display_elements()
{
    return display_elements | boost::adaptors::transformed(extract_disp_el);
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

std::vector<std::shared_ptr<display::Descriptible> > DisplayDB::get_selected(const geo::Point& p1, const geo::Point& p2, int zoom)
{
    double left = std::min(p1.lon, p2.lon);
    double right = std::max(p1.lon, p2.lon);
    double lower = std::min(p1.lat, p2.lat);
    double higher = std::max(p1.lat, p2.lat);

    std::vector<osm::Edge> edges;
    auto& stmt = coll.get_select_statement(zoom);
    stmt.execute(left, lower, right, higher);
    for (int i = 0; i < stmt.row_count(); ++i)
    {
        int64_t wid, sid, eid;
        int ssq, esq;
        std::tie(wid, ssq, sid, esq, eid) = stmt.get_row(i);
        edges.push_back(osm::Edge(osm::Node(sid), ssq, osm::Node(eid), esq, osm::Way(wid)));
    }

    return transl->translate(edges);
}

double DisplayDB::center_lat()
{
    return clat;
}

double DisplayDB::center_lon()
{
    return clon;
}

int DisplayDB::get_min_zoom()
{
    return minz;
}

int DisplayDB::get_max_zoom()
{
    return maxz;
}

} /* namespace display */
