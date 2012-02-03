/*
 * WayLister.cpp
 *
 *  Created on: Jan 12, 2012
 *      Author: martin
 */

#include "WayLister.h"
#include <boost/property_tree/xml_parser.hpp>
#include "../sqllib/sqllib.h"
#include <cmath>

namespace osmdb
{

WayLister::WayLister(OsmDatabase& db, std::multimap<std::string, std::string> const& attributes, unsigned int fetch_size):
    db(db),
    done(false),
    fetch_size(fetch_size)
{
    boost::property_tree::ptree ptree = get_entries(attributes);
    get_way_descr = psql::Cursor<psql::BindTypes<>, psql::RetTypes<int64_t, int64_t, double, double, int64_t, std::string, std::string, int> >(db.get_db(), "wayred_crs", sqllib::get_wayreduction_select(ptree, db.get_db()));
    get_way_descr.open();
}

std::map<osm::Way, std::multimap<osm::Node, osm::Way, osm::LtByID>, osm::LtByID> const& WayLister::get_current_connected_ways() const
{
    return current_connected_ways;
}

void WayLister::next()
{
    current_connected_ways.clear();
    get_way_descr.fetch(fetch_size);
    if (get_way_descr.get_buffer().size() < fetch_size)
    {
        done = true;
    }
    std::vector<osm::Way> cross_ways;
    osm::Way last_cross_way(-1);
    auto const& buf = get_way_descr.get_buffer();
    std::multimap<osm::Node, osm::Way, osm::LtByID> conn_ways_for_way;

    osm::Node last_node(-1);

    osm::Way way(-1);
    unsigned int max = buf.size() + rest.size();
    if (done)
        max++;
    unsigned int last_wayid_change = 0;
    unsigned int rest_size = rest.size();

    //loop through returned rows and fill current_ways and current_connected_ways
    for (unsigned int i = 0; i < max; ++i)
    {
        int64_t wid, cwid, nid;
        double lon, lat;
        std::string key, val;
        int sqno;
        if (i < rest_size)
            std::tie(wid, nid, lon, lat, cwid, key, val, sqno) = rest[i];
        else if (i < buf.size() + rest_size)
            std::tie(wid, nid, lon, lat, cwid, key, val, sqno) = buf[i-rest_size];
        else
            std::tie(wid, nid, lon, lat, cwid, key, val, sqno) = std::make_tuple(-1, -1, 0, 0, -1, "", "", 0);
        //new way
        if (way.id != wid)
        {
            if (way.id != -1)
            {
                if (last_cross_way.id != -1)
                {
                    cross_ways.push_back(last_cross_way);
                }
                if (last_node.id != -1)
                {
                    for (unsigned int j = 0; j < cross_ways.size(); ++j)
                        conn_ways_for_way.insert(std::pair<osm::Node, osm::Way>(last_node, cross_ways[j]));
                    way.nodes.push_back(last_node);
                }
                current_connected_ways.insert(std::make_pair(way, conn_ways_for_way));
            }
            conn_ways_for_way.clear();
            way.id = wid;
            way.nodes.clear();
            last_node.id = -1;
            last_cross_way.id = -1;
            last_wayid_change = i;
        }
        //new node
        if (last_node.id != nid)
        {
            if (last_node.id != -1)
            {
                way.nodes.push_back(last_node);
                if (last_cross_way.id != -1)
                    cross_ways.push_back(last_cross_way);
                for (unsigned int j = 0; j < cross_ways.size(); ++j)
                    conn_ways_for_way.insert(std::pair<osm::Node, osm::Way>(last_node, cross_ways[j]));
            }
            last_node.id = nid;
            last_node.position.lon = lon;
            last_node.position.lat = lat;
            cross_ways.clear();
            last_cross_way.id = -1;

        }
        //new cross way
        if (last_cross_way.id != cwid)
        {
            if (last_cross_way.id != -1)
                cross_ways.push_back(last_cross_way);
            last_cross_way.tags.clear();
            last_cross_way.id = cwid;
        }
        if (key != "")
            last_cross_way.tags.insert(osm::Tag(key, val));
    }

    //fix rest
    if (last_wayid_change != 0)
        rest.clear();
    for (unsigned int i = (unsigned int)std::max(0, (int)last_wayid_change - (int)rest_size); i < buf.size(); ++i)
    {
        rest.push_back(buf[i]);
    }
}

void WayLister::reset()
{
    get_way_descr.close();
    get_way_descr.open();
}

bool WayLister::end()
{
    return done;
}

boost::property_tree::ptree WayLister::get_entries(std::multimap<std::string, std::string> const& attributes)
{
    boost::property_tree::ptree ret;
    boost::property_tree::ptree entries;
    for (auto it = attributes.begin(); it != attributes.end(); ++it)
    {
        boost::property_tree::ptree entry;
        boost::property_tree::ptree kv;
        kv.put("key", it->first);
        kv.put("value", it->second);
        entry.put_child("elements.el", kv);
        entries.add_child("entry", entry);
    }
    ret.add_child("entries", entries);
    return ret;
}

WayLister::~WayLister()
{
}

} /* namespace osmdb */
