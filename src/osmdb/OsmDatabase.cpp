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

void OsmDatabase::create_indexes_and_keys()
{
    create_primary_keys();
    create_foreign_keys();
    create_indexes();
}

void OsmDatabase::create_indexes()
{
    sqllib::get_create_nodes_loc_index(db).execute();
    sqllib::get_create_edges_location_index(db).execute();
    sqllib::get_create_nodeattr_keyval_index(db).execute();
    sqllib::get_create_wayattr_keyval_index(db).execute();
    sqllib::get_create_relattr_keyval_index(db).execute();
}

void OsmDatabase::create_foreign_keys()
{
    sqllib::get_create_edges_startnode_fkey(db).execute();
    sqllib::get_create_edges_endnode_fkey(db).execute();
    sqllib::get_create_edges_wayid_fkey(db).execute();
    sqllib::get_create_waymembers_way_fkey(db).execute();
    sqllib::get_create_waymembers_relation_fkey(db).execute();
    sqllib::get_create_nodemembers_node_fkey(db).execute();
    sqllib::get_create_nodemembers_relation_fkey(db).execute();
    sqllib::get_create_waynodes_node_fkey(db).execute();
    sqllib::get_create_waynodes_way_fkey(db).execute();
    sqllib::get_create_nodeattributes_nodes_fkey(db).execute();
    sqllib::get_create_wayattributes_ways_fkey(db).execute();
    sqllib::get_create_relationattributes_relation_fkey(db).execute();
    sqllib::get_create_relmembers_child_fkey(db).execute();
    sqllib::get_create_relmembers_parent_fkey(db).execute();
}

void OsmDatabase::create_primary_keys()
{
    sqllib::get_create_nodes_pkey(db).execute();
    sqllib::get_create_ways_pkey(db).execute();
    sqllib::get_create_relations_pkey(db).execute();
    sqllib::get_create_edges_pkey(db).execute();
    sqllib::get_create_waymembers_pkey(db).execute();
    sqllib::get_create_nodemembers_pkey(db).execute();
    sqllib::get_create_waynodes_pkey(db).execute();
    sqllib::get_create_nodeattributes_pkey(db).execute();
    sqllib::get_create_wayattributes_pkey(db).execute();
    sqllib::get_create_relationattributes_pkey(db).execute();
    sqllib::get_create_relmembers_pkey(db).execute();
}

void OsmDatabase::drop_indexes_and_keys()
{
    drop_indexes();
    drop_foreign_keys();
    drop_primary_keys();
}

void OsmDatabase::drop_indexes()
{
    sqllib::get_drop_relattr_keyval_index(db).execute();
    sqllib::get_drop_wayattr_keyval_index(db).execute();
    sqllib::get_drop_nodeattr_keyval_index(db).execute();
    sqllib::get_drop_edges_location_index(db).execute();
    sqllib::get_drop_nodes_loc_index(db).execute();
}

void OsmDatabase::drop_foreign_keys()
{
    sqllib::get_drop_relationattributes_relation_fkey(db).execute();
    sqllib::get_drop_wayattributes_ways_fkey(db).execute();
    sqllib::get_drop_nodeattributes_nodes_fkey(db).execute();
    sqllib::get_drop_waynodes_way_fkey(db).execute();
    sqllib::get_drop_waynodes_node_fkey(db).execute();
    sqllib::get_drop_nodemembers_relation_fkey(db).execute();
    sqllib::get_drop_nodemembers_node_fkey(db).execute();
    sqllib::get_drop_waymembers_relation_fkey(db).execute();
    sqllib::get_drop_waymembers_way_fkey(db).execute();
    sqllib::get_drop_edges_wayid_fkey(db).execute();
    sqllib::get_drop_edges_endnode_fkey(db).execute();
    sqllib::get_drop_edges_startnode_fkey(db).execute();
    sqllib::get_drop_relmembers_child_fkey(db).execute();
    sqllib::get_drop_relmembers_parent_fkey(db).execute();
}

void OsmDatabase::drop_primary_keys()
{
    sqllib::get_drop_relmembers_pkey(db).execute();
    sqllib::get_drop_relationattributes_pkey(db).execute();
    sqllib::get_drop_wayattributes_pkey(db).execute();
    sqllib::get_drop_nodeattributes_pkey(db).execute();
    sqllib::get_drop_waynodes_pkey(db).execute();
    sqllib::get_drop_nodemembers_pkey(db).execute();
    sqllib::get_drop_waymembers_pkey(db).execute();
    sqllib::get_drop_nodes_pkey(db).execute();
    sqllib::get_drop_ways_pkey(db).execute();
    sqllib::get_drop_relations_pkey(db).execute();
    sqllib::get_drop_edges_pkey(db).execute();
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
    sqllib::get_create_relation_members(db).execute();
    sqllib::get_create_import_seq(db).execute();
    sqllib::get_create_import_table(db).execute();

}

}
