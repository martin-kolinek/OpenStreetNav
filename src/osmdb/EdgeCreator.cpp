/*
 * EdgeCreator.cpp
 *
 *  Created on: Mar 18, 2012
 *      Author: martin
 */

#include "EdgeCreator.h"
#include "../sqllib/sqllib.h"

namespace osmdb
{

EdgeCreator::EdgeCreator(OsmDatabase& db, boost::property_tree::ptree entries):
    db(db),
    entries(entries)
{
}

void EdgeCreator::create_tables()
{
    sqllib::get_create_edges_table(db.get_db()).execute();
}

void EdgeCreator::create_keys_and_indexes()
{
    sqllib::get_create_edges_pkey(db.get_db()).execute();
    sqllib::get_cluster_edges(db.get_db()).execute();
    sqllib::get_create_edges_startnode_fkey(db.get_db()).execute();
    sqllib::get_create_edges_endnode_fkey(db.get_db()).execute();
    sqllib::get_create_edges_location_index(db.get_db()).execute();
    sqllib::get_create_edges_start_waynode_fkey(db.get_db()).execute();
    sqllib::get_create_edges_end_waynode_fkey(db.get_db()).execute();
}

void EdgeCreator::insert_data()
{
    sqllib::get_edges_insert(entries, db.get_db()).execute();
}

void EdgeCreator::drop_keys_and_indexes()
{
    sqllib::get_create_edges_pkey(db.get_db()).execute();
    sqllib::get_create_edges_startnode_fkey(db.get_db()).execute();
    sqllib::get_create_edges_endnode_fkey(db.get_db()).execute();
    sqllib::get_create_edges_location_index(db.get_db()).execute();
    sqllib::get_create_edges_start_waynode_fkey(db.get_db()).execute();
    sqllib::get_create_edges_end_waynode_fkey(db.get_db()).execute();
}

EdgeCreator::~EdgeCreator()
{
}

} /* namespace osmdb */
