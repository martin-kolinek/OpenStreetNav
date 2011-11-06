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
#include <vector>
#include <cstdint>
#include <tuple>
#include <cassert>

namespace sqlite
{

/**
 * \class Statement
 *
 * Sqlite statement wrapper. Finalizes statement on destruction.
 */
class Statement
{
private:
    template <class T>
    class ColType
    {
    public:
        T get_data(Statement& st, int col_index)
        {
            return T();
        }
    };
    friend Statement::ColType<double> coldouble();
    friend Statement::ColType<int> colint();
    friend Statement::ColType<int64_t> colint64();
    friend Statement::ColType<std::string> colstr();

    template <class T>
    class BindWrap
    {
    };

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

    /**
     *
     * @param types types of returned columns (see \ref colint, \ref coldouble, \ref colint64, \ref colstring)
     * @return current row as tuple
     */
    template<typename... Args> std::tuple<Args...> get_row(ColType<Args>... types)
    {
        return get_row_internal(0, types...);
    }

    /**
     *
     * @param types the same as in \ref get_row
     * @return all remaining rows as tuples
     */
    template<typename... Args> std::vector<std::tuple<Args...> > get_remaining_rows(ColType<Args>... types)
    {
        std::vector<std::tuple<Args...> > ret;
        while (!done())
        {
            if (has_row())
                ret.push_back(get_row(types...));
            step();
        }
        return ret;
    }

    /**
     * Binds more values to parameters
     * Starts with index 1
     * @param values values to bind
     */
    template<typename... Args> void bind(Args... values)
    {
        bind_internal(1, values...);
    }

    /**
     * Column type int
     */
    static ColType<int> colint;
    /**
     * Column type double
     */
    static ColType<double> coldouble;
    /**
     * Column type int64
     */
    static ColType<int64_t> colint64;
    /**
     * Column type string
     */
    static ColType<std::string> colstring;

private:
    void reset_internal();
    void reset_nothrow();
    sqlite3_stmt* stmt;
    Database* db;
    void check_value_conditions(int col);
    int cols;
    bool hrow;
    bool dn;

    std::tuple<> get_row_internal(int)
    {
        return std::tuple<>();
    }
    template<typename Head, typename... Tail> std::tuple<Head, Tail...> get_row_internal(int i, ColType<Head> h, ColType<Tail>... t)
    {
        std::tuple<Tail...> tail = get_row_internal(i + 1, t...);
        Head head = h.get_data(*this, i);
        return std::tuple_cat(std::make_tuple(head), tail);
    }

    void bind_internal(int)
    {
    }

    template<typename Head, typename... Tail> void bind_internal(int i, Head h, Tail... t)
    {
        BindWrap<Head> bw;
        bw.bind(i, h, *this);
        bind_internal(i + 1, t...);
    }

};

/**
 * Executes simple sql statement with no result
 * @param sql sql statement
 * @param db database connection
 */
void execute_sql(std::string sql, Database& db);

/**
 * Executes sql query
 * @param sql sql query
 * @param db database connection
 * @param types same as in \ref Statement::get_row
 * @return vector of returned rows as tuples
 */
template<typename... Args> std::vector<std::tuple<Args...> > query_sql(std::string sql, Database& db, Statement::ColType<Args>... types)
{
    Statement st(sql, db);
    return st.get_remaining_rows(types...);
}

template<>
class Statement::ColType<int>
{
public:
    int get_data(Statement& st, int col_index)
    {
        return st.val_int(col_index);
    }
};

template<>
class Statement::ColType<double>
{
public:
    double get_data(Statement& st, int col_index)
    {
        return st.val_double(col_index);
    }
};

template<>
class Statement::ColType<int64_t>
{
public:
    int64_t get_data(Statement& st, int col_index)
    {
        return st.val_int64(col_index);
    }
};

template<>
class Statement::ColType<std::string>
{
public:
    std::string get_data(Statement& st, int col_index)
    {
        return st.val_string(col_index);
    }
};

Statement::ColType<double> coldouble();

Statement::ColType<int> colint();

Statement::ColType<int64_t> colint64();

Statement::ColType<std::string> colstr();

template<>
class Statement::BindWrap<int>
{
public:
    void bind(int index, int val, Statement& st)
    {
        st.bind_int(index, val);
    }
};

template<>
class Statement::BindWrap<int64_t>
{
public:
    void bind(int index, int64_t val, Statement& st)
    {
        st.bind_int64(index, val);
    }
};

template<>
class Statement::BindWrap<std::string>
{
public:
    void bind(int index, std::string const& val, Statement& st)
    {
        st.bind_string(index, val);
    }
};

template<>
class Statement::BindWrap<double>
{
public:
    void bind(int index, double val, Statement& st)
    {
        st.bind_double(index, val);
    }
};

}

#endif /* STATEMENT_H_ */
