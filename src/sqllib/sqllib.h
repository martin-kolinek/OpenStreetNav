/*
 * sqllib.h
 *
 *  Created on: Nov 23, 2011
 *      Author: martin
 */

#ifndef SQLLIB_H_
#define SQLLIB_H_

#include "../psql/psql.h"
#include <string>

namespace sqllib
{

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_create_edges_endnode_fkey(psql::Database& db, bool named = false, std::string const& name = "");

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_create_edges_location_index(psql::Database& db, bool named = false, std::string const& name = "");

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_create_edges_pkey(psql::Database& db, bool named = false, std::string const& name = "");

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_create_edges_startnode_fkey(psql::Database& db, bool named = false, std::string const& name = "");

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_create_edges_table(psql::Database& db, bool named = false, std::string const& name = "");

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_create_edges_wayid_fkey(psql::Database& db, bool named = false, std::string const& name = "");

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_create_node_attributes(psql::Database& db, bool named = false, std::string const& name = "");

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_create_node_members(psql::Database& db, bool named = false, std::string const& name = "");

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_create_nodeattr_keyval_index(psql::Database& db, bool named = false, std::string const& name = "");

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_create_nodeattributes_nodes_fkey(psql::Database& db, bool named = false, std::string const& name = "");

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_create_nodeattributes_pkey(psql::Database& db, bool named = false, std::string const& name = "");

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_create_nodemembers_node_fkey(psql::Database& db, bool named = false, std::string const& name = "");

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_create_nodemembers_pkey(psql::Database& db, bool named = false, std::string const& name = "");

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_create_nodemembers_relation_fkey(psql::Database& db, bool named = false, std::string const& name = "");

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_create_nodes_loc_index(psql::Database& db, bool named = false, std::string const& name = "");

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_create_nodes_pkey(psql::Database& db, bool named = false, std::string const& name = "");

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_create_nodes_table(psql::Database& db, bool named = false, std::string const& name = "");

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_create_relation_attributes(psql::Database& db, bool named = false, std::string const& name = "");

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_create_relationattributes_pkey(psql::Database& db, bool named = false, std::string const& name = "");

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_create_relationattributes_relation_fkey(psql::Database& db, bool named = false, std::string const& name = "");

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_create_relationattributess_pkey(psql::Database& db, bool named = false, std::string const& name = "");

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_create_relations_pkey(psql::Database& db, bool named = false, std::string const& name = "");

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_create_relations_table(psql::Database& db, bool named = false, std::string const& name = "");

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_create_relattr_keyval_index(psql::Database& db, bool named = false, std::string const& name = "");

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_create_test_table(psql::Database& db, bool named = false, std::string const& name = "");

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_create_toshow_table(psql::Database& db, bool named = false, std::string const& name = "");

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_create_way_attributes(psql::Database& db, bool named = false, std::string const& name = "");

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_create_way_members_table(psql::Database& db, bool named = false, std::string const& name = "");

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_create_wayattr_keyval_index(psql::Database& db, bool named = false, std::string const& name = "");

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_create_wayattributes_pkey(psql::Database& db, bool named = false, std::string const& name = "");

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_create_wayattributes_ways_fkey(psql::Database& db, bool named = false, std::string const& name = "");

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_create_waymembers_pkey(psql::Database& db, bool named = false, std::string const& name = "");

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_create_waymembers_relation_fkey(psql::Database& db, bool named = false, std::string const& name = "");

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_create_waymembers_way_fkey(psql::Database& db, bool named = false, std::string const& name = "");

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_create_waynodes_node_fkey(psql::Database& db, bool named = false, std::string const& name = "");

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_create_waynodes_pkey(psql::Database& db, bool named = false, std::string const& name = "");

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_create_waynodes_table(psql::Database& db, bool named = false, std::string const& name = "");

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_create_waynodes_way_fkey(psql::Database& db, bool named = false, std::string const& name = "");

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_create_ways_pkey(psql::Database& db, bool named = false, std::string const& name = "");

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_create_ways_table(psql::Database& db, bool named = false, std::string const& name = "");

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_drop_edges_endnode_fkey(psql::Database& db, bool named = false, std::string const& name = "");

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_drop_edges_location_index(psql::Database& db, bool named = false, std::string const& name = "");

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_drop_edges_pkey(psql::Database& db, bool named = false, std::string const& name = "");

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_drop_edges_startnode_fkey(psql::Database& db, bool named = false, std::string const& name = "");

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_drop_edges_wayid_fkey(psql::Database& db, bool named = false, std::string const& name = "");

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_drop_nodeattr_keyval_index(psql::Database& db, bool named = false, std::string const& name = "");

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_drop_nodeattributes_nodes_fkey(psql::Database& db, bool named = false, std::string const& name = "");

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_drop_nodeattributes_pkey(psql::Database& db, bool named = false, std::string const& name = "");

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_drop_nodemembers_node_fkey(psql::Database& db, bool named = false, std::string const& name = "");

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_drop_nodemembers_pkey(psql::Database& db, bool named = false, std::string const& name = "");

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_drop_nodemembers_relation_fkey(psql::Database& db, bool named = false, std::string const& name = "");

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_drop_nodes_loc_index(psql::Database& db, bool named = false, std::string const& name = "");

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_drop_nodes_pkey(psql::Database& db, bool named = false, std::string const& name = "");

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_drop_relationattributes_pkey(psql::Database& db, bool named = false, std::string const& name = "");

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_drop_relationattributes_relation_fkey(psql::Database& db, bool named = false, std::string const& name = "");

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_drop_relations_pkey(psql::Database& db, bool named = false, std::string const& name = "");

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_drop_relattr_keyval_index(psql::Database& db, bool named = false, std::string const& name = "");

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_drop_wayattr_keyval_index(psql::Database& db, bool named = false, std::string const& name = "");

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_drop_wayattributes_pkey(psql::Database& db, bool named = false, std::string const& name = "");

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_drop_wayattributes_ways_fkey(psql::Database& db, bool named = false, std::string const& name = "");

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_drop_waymembers_pkey(psql::Database& db, bool named = false, std::string const& name = "");

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_drop_waymembers_relation_fkey(psql::Database& db, bool named = false, std::string const& name = "");

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_drop_waymembers_way_fkey(psql::Database& db, bool named = false, std::string const& name = "");

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_drop_waynodes_node_fkey(psql::Database& db, bool named = false, std::string const& name = "");

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_drop_waynodes_pkey(psql::Database& db, bool named = false, std::string const& name = "");

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_drop_waynodes_way_fkey(psql::Database& db, bool named = false, std::string const& name = "");

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_drop_ways_pkey(psql::Database& db, bool named = false, std::string const& name = "");

psql::Statement<psql::BindTypes<int64_t, int64_t, int64_t>, psql::RetTypes<>> get_insert_edge(psql::Database& db, bool named = false, std::string const& name = "");

psql::Statement<psql::BindTypes<int64_t, double, double>, psql::RetTypes<>> get_insert_node(psql::Database& db, bool named = false, std::string const& name = "");

psql::Statement<psql::BindTypes<int64_t, std::string, std::string>, psql::RetTypes<>> get_insert_node_attr(psql::Database& db, bool named = false, std::string const& name = "");

psql::Statement<psql::BindTypes<int, std::string, int64_t>, psql::RetTypes<>> get_insert_test_table(psql::Database& db, bool named = false, std::string const& name = "");

psql::Statement<psql::BindTypes<std::string, std::string, int>, psql::RetTypes<>> get_insert_toshow(psql::Database& db, bool named = false, std::string const& name = "");

psql::Statement<psql::BindTypes<int64_t>, psql::RetTypes<>> get_insert_way(psql::Database& db, bool named = false, std::string const& name = "");

psql::Statement<psql::BindTypes<int64_t, std::string, std::string>, psql::RetTypes<>> get_insert_way_attr(psql::Database& db, bool named = false, std::string const& name = "");

psql::Statement<psql::BindTypes<int64_t, int64_t, int>, psql::RetTypes<>> get_insert_way_node(psql::Database& db, bool named = false, std::string const& name = "");

psql::Statement<psql::BindTypes<>, psql::RetTypes<double, double, double, double>> get_select_bounds(psql::Database& db, bool named = false, std::string const& name = "");

psql::Statement<psql::BindTypes<int, double, double, double, double>, psql::RetTypes<double, double, double, double>> get_select_edges_in_box(psql::Database& db, bool named = false, std::string const& name = "");

psql::Statement<psql::BindTypes<int, double, double, double, double>, psql::RetTypes<int64_t, std::string, std::string>> get_select_node_descr_in_box(psql::Database& db, bool named = false, std::string const& name = "");

psql::Statement<psql::BindTypes<int, double, double, double, double>, psql::RetTypes<double, double>> get_select_nodes_in_box(psql::Database& db, bool named = false, std::string const& name = "");

psql::Statement<psql::BindTypes<int, double, double, double, double>, psql::RetTypes<int64_t, std::string, std::string>> get_select_way_descr_in_box(psql::Database& db, bool named = false, std::string const& name = "");

psql::Statement<psql::BindTypes<int>, psql::RetTypes<int, std::string, int64_t>> get_test_select(psql::Database& db, bool named = false, std::string const& name = "");


}

#endif
