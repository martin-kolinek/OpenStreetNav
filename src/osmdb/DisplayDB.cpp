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

DisplayDB::DisplayDB(OsmDatabase& db)
    : db(db),
      edge_st(sqllib::get_select_edges_in_box(db.get_db())),
      node_st(sqllib::get_select_nodes_in_box(db.get_db())),
      to_show_ins(sqllib::get_insert_toshow(db.get_db()))
{
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
    edge_st.execute(zoom, p1.lon, p1.lat, p2.lon, p2.lat);
    node_st.execute(zoom, p1.lon, p1.lat, p2.lon, p2.lat);
    for (int i = 0; i < edge_st.row_count(); ++i)
    {
        double a, b, c, d;
        std::tie(a, b, c, d) = edge_st.get_row(i);
        edges.push_back(geo::Edge(geo::Point(a, b), geo::Point(c, d)));
    }
    for (int i = 0; i < node_st.row_count(); ++i)
    {
        double a, b;
        std::tie(a, b) = node_st.get_row(i);
        points.push_back(geo::Point(a, b));
    }
}

void DisplayDB::set_to_show(const std::string& key, const std::string& value, int minzoom, int maxzoom)
{
    assert(minzoom <= maxzoom);
    for (int i = minzoom; i <= maxzoom; ++i)
    {
        to_show_ins.execute(key, value, i);
    }
}

} /* namespace display */
