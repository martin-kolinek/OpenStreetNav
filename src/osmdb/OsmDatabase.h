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

};

} /* namespace osmdb */
#endif /* OSMDATABASE_H_ */
