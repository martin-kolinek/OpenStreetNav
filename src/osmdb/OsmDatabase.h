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
 * Database connection which automatically creates tables for storing osm objects
 */
class OsmDatabase
{
public:
    /**
     *
     * @param file filename where to store sqlite database
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
    void create_indexes();

    /**
     * Drops all indexes and keys. Call before inserting large amounts of data
     */
    void drop_indexes();

    /**
     * Creates needed tables in the database
     */
    void create_tables();
private:
    psql::Database& db;
};

} /* namespace osmdb */
#endif /* OSMDATABASE_H_ */
