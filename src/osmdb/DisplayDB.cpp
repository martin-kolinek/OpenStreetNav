/*
 * DisplayDB.cpp
 *
 *  Created on: Nov 12, 2011
 *      Author: martin
 */

#include "DisplayDB.h"

namespace osmdb
{

DisplayDB::DisplayDB(OsmDatabase& db)
    : db(db)
{
}

DisplayDB::~DisplayDB()
{
}

const std::vector<osm::Edge> & DisplayDB::get_edges()
{
    return edges;
}

std::unordered_map<int64_t, osm::Node> const& DisplayDB::get_nodes()
{
    return nodes;
}

const std::unordered_set<int64_t> & DisplayDB::get_free_nodes()
{
    return free_nodes;
}

OsmDatabase& DisplayDB::get_db()
{
    return db;
}

void DisplayDB::set_to_show(std::string const& , std::string const& , int , int )
{

}

void DisplayDB::set_bounds(const geo::Point& , const geo::Point& , int )
{
}

} /* namespace display */
