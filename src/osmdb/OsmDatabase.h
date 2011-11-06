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
private:
    sqlite::Database db;
    std::string nodes_create;
    std::string ways_create;
    std::string edges_create;
    std::string relations_create;
    std::string relation_contents_create;
    std::string attributes_create;
    std::string attr_index1;
    std::string attr_index2;
    std::string attr_index3;
    std::string edge_index_start;
    std::string edge_index_end;
    std::string edge_index_way;
    std::string node_index_lat;
    std::string node_index_lon;
    std::string rel_cont_index_obj;
    std::string rel_cont_index_rel;
    std::string rel_cont_index_role;
    std::vector<std::string> indexes;
    std::vector<std::string> tables;

};

} /* namespace osmdb */
#endif /* OSMDATABASE_H_ */
