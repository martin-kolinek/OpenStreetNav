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

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_create_edges_location_index(psql::Database& db, bool named = false, std::string const& name = "");

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_create_edges_table(psql::Database& db, bool named = false, std::string const& name = "");

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_create_node_attributes(psql::Database& db, bool named = false, std::string const& name = "");

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_create_node_members(psql::Database& db, bool named = false, std::string const& name = "");

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_create_nodes_loc_index(psql::Database& db, bool named = false, std::string const& name = "");

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_create_nodes_table(psql::Database& db, bool named = false, std::string const& name = "");

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_create_relation_attributes(psql::Database& db, bool named = false, std::string const& name = "");

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_create_relations_table(psql::Database& db, bool named = false, std::string const& name = "");

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_create_test_table(psql::Database& db, bool named = false, std::string const& name = "");

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_create_toshow_table(psql::Database& db, bool named = false, std::string const& name = "");

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_create_way_attributes(psql::Database& db, bool named = false, std::string const& name = "");

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_create_way_members_table(psql::Database& db, bool named = false, std::string const& name = "");

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_create_waynodes_table(psql::Database& db, bool named = false, std::string const& name = "");

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_create_ways_table(psql::Database& db, bool named = false, std::string const& name = "");

psql::Statement<psql::BindTypes<int64_t, int64_t, int64_t>, psql::RetTypes<>> get_insert_edge(psql::Database& db, bool named = false, std::string const& name = "");

psql::Statement<psql::BindTypes<int64_t, double, double>, psql::RetTypes<>> get_insert_node(psql::Database& db, bool named = false, std::string const& name = "");

psql::Statement<psql::BindTypes<int64_t, std::string, std::string>, psql::RetTypes<>> get_insert_node_attr(psql::Database& db, bool named = false, std::string const& name = "");

psql::Statement<psql::BindTypes<int, std::string, int64_t>, psql::RetTypes<>> get_insert_test_table(psql::Database& db, bool named = false, std::string const& name = "");

psql::Statement<psql::BindTypes<std::string, std::string, int>, psql::RetTypes<>> get_insert_toshow(psql::Database& db, bool named = false, std::string const& name = "");

psql::Statement<psql::BindTypes<int64_t>, psql::RetTypes<>> get_insert_way(psql::Database& db, bool named = false, std::string const& name = "");

psql::Statement<psql::BindTypes<int64_t, std::string, std::string>, psql::RetTypes<>> get_insert_way_attr(psql::Database& db, bool named = false, std::string const& name = "");

psql::Statement<psql::BindTypes<int64_t, int64_t, int>, psql::RetTypes<>> get_insert_way_node(psql::Database& db, bool named = false, std::string const& name = "");

psql::Statement<psql::BindTypes<int, double, double, double, double>, psql::RetTypes<double, double, double, double>> get_select_edges_in_box(psql::Database& db, bool named = false, std::string const& name = "");

psql::Statement<psql::BindTypes<int, double, double, double, double>, psql::RetTypes<double, double>> get_select_nodes_in_box(psql::Database& db, bool named = false, std::string const& name = "");

psql::Statement<psql::BindTypes<int>, psql::RetTypes<int, std::string, int64_t>> get_test_select(psql::Database& db, bool named = false, std::string const& name = "");


}

#endif
