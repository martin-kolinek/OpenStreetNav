/*
 * OsmDatabase.h
 *
 *  Created on: Nov 5, 2011
 *      Author: martin
 */

#ifndef OSMDATABASE_H_
#define OSMDATABASE_H_

#include "../psql/psql.h"
#include "../elements/osmelements.h"

namespace osmdb
{

/**
 * \class OsmDatabase
 * Class responsible for creating database schema.
 */
class OsmDatabase
{
public:
    /**
     * Constructor
     * @param db underlying database connection
     */
    OsmDatabase(psql::Database& db);
    virtual ~OsmDatabase();

    /**
     *
     * @return underlying database connection
     */
    psql::Database& get_db();
    /**
     * Creates indexes and keys, this does not happen right away for optimization purposes.
     * Call after inserting most of the data.
     */
    void create_indexes_and_keys();

    void create_indexes();

    void create_foreign_keys();

    void create_primary_keys();

    /**
     * Drops all indexes and keys. Call before inserting large amounts of data
     */
    void drop_indexes_and_keys();

    void drop_indexes();

    void drop_foreign_keys();

    void drop_primary_keys();

    /**
     * Creates needed tables in the database
     */
    void create_tables();

    void create_edge_tables();

    void create_edge_primary_keys();

    void create_edge_foreign_keys();

    void create_edge_keys_and_indexes();

    void drop_edge_primary_keys();

    void drop_edge_foreign_keys();

    void drop_edge_keys_and_indexes();

private:
    psql::Database& db;
};

} /* namespace osmdb */
#endif /* OSMDATABASE_H_ */
