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
      to_show_ins(sqllib::get_insert_toshow(db.get_db())),
      way_desc(sqllib::get_select_way_descr_in_box(db.get_db())),
      node_desc(sqllib::get_select_node_descr_in_box(db.get_db()))
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
    double left = std::min(p1.lon, p2.lon);
    double right = std::max(p1.lon, p2.lon);
    double lower = std::min(p1.lat, p2.lat);
    double higher = std::max(p1.lat, p2.lat);
    edge_st.execute(zoom, left, lower, right, higher);
    node_st.execute(zoom, left, lower, right, higher);
    for (int i = 0; i < edge_st.row_count(); ++i)
    {
        double a, b, c, d;
        std::tie(a, b, c, d) = edge_st.get_row(i);
        edges.push_back(geo::Edge(geo::Point(b, a), geo::Point(d, c)));
    }
    for (int i = 0; i < node_st.row_count(); ++i)
    {
        double a, b;
        std::tie(b, a) = node_st.get_row(i);
        points.push_back(geo::Point(a, b));
    }
}

std::vector<std::unique_ptr<osm::Element> > DisplayDB::get_selected(const geo::Point& p1, const geo::Point& p2, int zoom)
{
    double left = std::min(p1.lon, p2.lon);
    double right = std::max(p1.lon, p2.lon);
    double lower = std::min(p1.lat, p2.lat);
    double higher = std::max(p1.lat, p2.lat);
    node_desc.execute(zoom, left, lower, right, higher);
    way_desc.execute(zoom, left, lower, right, higher);
    std::vector<std::unique_ptr<osm::Element> > ret;
    osm::Node n(-1, 0, 0);
    for (int i = 0; i < node_desc.row_count(); ++i)
    {
        int64_t id;
        std::string key, val;
        std::tie(id, key, val) = node_desc.get_row(i);
        if (id != n.id)
        {
            if (n.id != -1)
                ret.push_back(std::unique_ptr<osm::Element>(new osm::Node(n)));
            n.tags.clear();
            n.id = id;
        }
        n.tags.push_back(osm::Tag(key, val));
    }
    if (n.id != -1)
        ret.push_back(std::unique_ptr<osm::Element>(new osm::Node(n)));
    osm::Way w(-1);
    for (int i = 0; i < node_desc.row_count(); ++i)
    {
        int64_t id;
        std::string key, val;
        std::tie(id, key, val) = node_desc.get_row(i);
        if (id != w.id)
        {
            if (w.id != -1)
                ret.push_back(std::unique_ptr<osm::Element>(new osm::Way(w)));
            w.tags.clear();
            w.id = id;
        }
        w.tags.push_back(osm::Tag(key, val));
    }
    if (w.id != -1)
        ret.push_back(std::unique_ptr<osm::Element>(new osm::Way(w)));
    return ret;
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
