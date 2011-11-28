/*
 * OsmDatabase.cpp
 *
 *  Created on: Nov 5, 2011
 *      Author: martin
 */

#include "OsmDatabase.h"
#include "../sqllib/sqllib.h"
#include <functional>
#include <vector>

namespace osmdb
{

OsmDatabase::OsmDatabase(psql::Database& db):
    db(db)
{
}

OsmDatabase::~OsmDatabase()
{
}

psql::Database& OsmDatabase::get_db()
{
    return db;
}

void OsmDatabase::create_indexes()
{
    sqllib::get_create_nodes_loc_index(db).execute();
    sqllib::get_create_edges_location_index(db).execute();
}

void OsmDatabase::create_tables()
{
    sqllib::get_create_nodes_table(db).execute();
    sqllib::get_create_ways_table(db).execute();
    sqllib::get_create_edges_table(db).execute();
    sqllib::get_create_waynodes_table(db).execute();
    sqllib::get_create_node_attributes(db).execute();
    sqllib::get_create_way_attributes(db).execute();
    sqllib::get_create_relations_table(db).execute();
    sqllib::get_create_node_members(db).execute();
    sqllib::get_create_way_members_table(db).execute();
    sqllib::get_create_relation_attributes(db).execute();
    sqllib::get_create_toshow_table(db).execute();

}

}
