/*
 * Statement.h
 *
 *  Created on: Nov 2, 2011
 *      Author: martin
 */

#ifndef STATEMENT_H_
#define STATEMENT_H_

#include <sqlite3.h>
#include "Database.h"
#include <cstdint>

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

    /**
     *
     * @param col_index index of retrieved column
     * @return value of a column in current row as double
     */
    double val_double(int col_index);
    /**
     *
     * @param col_index index of retrieved column
     * @return value of a column in current row as int
     */
    int val_int(int col_index);
    /**
     *
     * @param col_index index of retrieved column
     * @return value of a column in current row as 64 bit integer
     */
    int64_t val_int64(int col_index);
    /**
     *
     * @param col_index index of retrieved column
     * @return value of a column in current row as string
     */
    std::string val_string(int col_index);

    /**
     * Binds a value to prepared statement parameter
     * @param param_index
     * @param value
     */
    void bind_double(int param_index, double value);
    /**
     * Binds a value to prepared statement parameter
     * @param param_index
     * @param value
     */
    void bind_int(int param_index, int value);
    /**
     * Binds a value to prepared statement parameter
     * @param param_index
     * @param value
     */
    void bind_int64(int param_index, int64_t value);
    /**
     * Binds a value to prepared statement parameter
     * @param param_index
     * @param value
     */
    void bind_string(int param_index, std::string const& value);

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
