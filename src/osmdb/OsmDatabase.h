/*
 * OsmDatabase.h
 *
 *  Created on: Nov 5, 2011
 *      Author: martin
 */

#ifndef OSMDATABASE_H_
#define OSMDATABASE_H_

#include "../sqlite/sqlitewrap.h"
#include "../elements/osmelements.h"

namespace osmdb
{

class OsmDatabase
{
public:
    OsmDatabase(std::string const& file);
    virtual ~OsmDatabase();
    std::string const nodes_table;
    std::string const ways_table;
    std::string const edges_table;
    std::string const relations_table;
    std::string const relation_contents_table;
    std::string const attributes_table;
    sqlite::Database& get_db();
    void create_indexes();
private:
    sqlite::Database db;
    std::string const nodes_create;
    std::string const ways_create;
    std::string const edges_create;
    std::string const relations_create;
    std::string const relation_contents_create;
    std::string const attributes_create;
    std::string const attr_index1;
    std::string const attr_index2;
    std::string const attr_index3;
    std::string const edge_index_start;
    std::string const edge_index_end;
    std::string const edge_index_way;
    std::string const node_index_lat;
    std::string const node_index_lon;
    std::string const rel_cont_index_obj;
    std::string const rel_cont_index_rel;
    std::string const rel_cont_index_role;
    std::string const nodes_test;
    std::string const ways_test;
    std::string const edge_test;
    std::string const relation_test;
    std::string const rel_contents_test;
    std::string const attributes_test;
    std::vector<std::string> const indexes;
    std::vector<std::string> const tables;
    std::vector<std::string> const checks;
    void create_tables();
    void check_tables();
};

} /* namespace osmdb */
#endif /* OSMDATABASE_H_ */
