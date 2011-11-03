/*
 * Statement.h
 *
 *  Created on: Nov 2, 2011
 *      Author: martin
 */

#include <sqlite3.h>
#include "Database.h"

#ifndef STATEMENT_H_
#define STATEMENT_H_

namespace sqlite
{
/**
 * \class Statement
 *
 * Sqlite statement wrapper. Finalizes statement on destruction.
 */
class Statement
{
public:
    /**
     * Constructs statement. Also registers the constructed statement with the database connection.
     * @param sql sql statement
     * @param db database connection
     */
    Statement(std::string const& sql, Database& db);
    virtual ~Statement();
    Statement(Statement const&) = delete;
    Statement& operator=(Statement const&) = delete;
    Statement(Statement && other);
    Statement& operator=(Statement && other);

    /**
     * @return whether the statement is done, as in all of it has been executed
     */
    bool done();
    /**
     *
     * @return whether row data can be queried using val_* functions
     */
    bool has_row();
    /**
     * Step the statement as in get next row or execute insert, update etc.
     */
    void step();
    void const* val_blob(int col_index);
    double val_double(int col_index);
    int val_int(int col_index);
    std::string val_string(int col_index);
    /**
     * Reset the statement. Resets done and has_row flags. If it is a query stepping will result in rows
     * being returned from beginning again. Otherwise it has no apparent reason to call this since step
     * can be called again even without reset.
     */
    void reset();
    /**
     * Finalizes the statement before destruction. No other function than finalize can be called on it
     * after this.
     */
    void finalize();
private:
    void reset_internal();
    void reset_nothrow();
    sqlite3_stmt* stmt;
    Database* db;
    void check_value_conditions(int col);
    int cols;
    bool hrow;
    bool dn;

};
}

#endif /* STATEMENT_H_ */
