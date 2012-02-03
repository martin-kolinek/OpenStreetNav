#ifndef PSQL_STATEMENT_H_
#define PSQL_STATEMENT_H_

#include "Database.h"
#include "PgSqlException.h"
#include "CopyTypes.h"
#include "BindTypes.h"
#include <libpqtypes.h>
#include <tuple>
#include "../util/util.h"

namespace psql
{

class IStatement
{
};

template<typename BindTypes>
PGresult* execBT(PGconn* conn, PGparam* param, std::string const& sql)
{
    auto res = PQparamExec(conn, param, sql.c_str(), 1);
    if (res == NULL)
        throw PgSqlException("Error retrieving statement result: " + std::string(PQgeterror()));
    return res;
}

template<>
PGresult* execBT<BindTypes<> >(PGconn* conn, PGparam*, std::string const& sql);

template<typename BindTypes>
PGresult* execPrepBT(PGconn* conn, PGparam* param, std::string const& name)
{
    auto res = PQparamExecPrepared(conn, param, name.c_str(), 1);
    if (res == NULL)
        throw PgSqlException("Error retrieving statement result: " + std::string(PQgeterror()));
    return res;
}

template<>
PGresult* execPrepBT<BindTypes<> >(PGconn* conn, PGparam*, std::string const& name);


/**
 * \class Statement
 * Class representing an executable sql statement. BindTypes, RetTypes and CopyTypes specify types to be used as
 * arguments to the query, the types of rows returned and types of rows to copy into database if the query
 * initiates copying respectively.
 */
template < typename BindTypes, typename RetTypes, typename CopyTypes = CopyTypes<> >
class Statement : IStatement
{
private:
    void check_param()
    {
        if (param == NULL)
            throw PgSqlException("Error creating PGparam object: " + std::string(PQgeterror()));
    }
public:
    /**
     * Constructs an empty statement
     */
    Statement():
        db(NULL),
        param(NULL),
        res(NULL),
        cp(false)
    {
    }

    /**
     * Constructs a statement representing sql query
     * @param sql query to execute
     * @param db connection to use
     */
    Statement(std::string const& sql, Database& db):
        db(&db),
        sql(sql),
        prep(false),
        param(PQparamCreate(db.get_db())),
        res(NULL),
        cp(false)
    {
        check_param();
    }
    /**
     * Constructs a server side prepared statement
     * @param name name of the statement
     * @param sql query to execute
     * @param db connection to use
     */
    Statement(std::string const& name, std::string const& sql, Database& db):
        db(&db),
        name(name),
        sql(sql),
        prep(true),
        param(PQparamCreate(db.get_db())),
        res(NULL),
        cp(false)
    {
        db.regist(name, sql, this);
        check_param();
    }
    ~Statement()
    {
        if (db == NULL)
            return;
        if (prep)
            db->unregist(name, this);
        if (res != NULL)
            PQclear(res);
        if (cp)
            end_copy();
        PQparamClear(param);
    }

    Statement& operator=(Statement const&) = delete;
    Statement(Statement<BindTypes, RetTypes, CopyTypes> const&) = delete;
    /**
     * Move assignment
     * @param other
     * @return *this
     */
    Statement& operator=(Statement && other)
    {
        if (db != NULL)
        {
            if (prep)
                db->unregist(name, this);
            if (res != NULL)
                PQclear(res);
        }
        db = other.db;
        other.db = NULL;
        res = other.res;
        prep = other.prep;
        sql = other.sql;
        name = other.name;
        param = other.param;
        cp = other.cp;
        return *this;
    }
    /**
     * Move construction
     * @param other
     */
    Statement(Statement && other):
        db(NULL),
        param(NULL),
        res(NULL)
    {
        *this = std::move(other);
    }

    /**
     * Execute the query.
     * Args need to correspond to supplied BindTypes
     * @param args arguments to query
     */
    template<typename... Args>
    void execute(Args... args)
    {
        if (res != NULL)
            PQclear(res);
        res = NULL;

        bt.put(param, args...);
        if (prep)
            res = execPrepBT<BindTypes>(db->get_db(), param, name);
        else
            res = execBT<BindTypes>(db->get_db(), param, sql);

        if (PQresultStatus(res) == PGRES_COPY_IN)
            cp = true;
    }

    /**
     *
     * @param row zero based index of row to retrieve
     * @return row from result with given index
     */
    typename RetTypes::RowType get_row(int row)
    {
        if (res == NULL)
            throw PgSqlException("get_values called with no result");
        return rt.get_values(res, row);
    }

    /**
     *
     * @return number of rows in result set
     */
    int row_count()
    {
        if (res == NULL)
            return 0;
        if (PQresultStatus(res) == PGRES_TUPLES_OK)
            return PQntuples(res);
        return 0;
    }

    /**
     *
     * @return number of rows affected by executing the statement
     */
    int64_t affected_rows()
    {
        if (res == NULL)
            return 0;
        std::string aff(PQcmdTuples(res));
        if (aff == "")
            return 0;
        return util::parse<int64_t>(aff);
    }

    /**
     *
     * @return whether statement execution started data copying into the database
     */
    bool copying()
    {
        return cp;
    }

    /**
     * Finish copying into the database.
     */
    void end_copy()
    {
        auto conn = db->get_db();
        cp = false;
        auto result = PQputCopyEnd(conn, NULL);
        if (result == 0)
            throw PgSqlException("Sorry copy for asynchronous connections is not implemented");
        if (result == -1)
            throw PgSqlException("Error sending end copy request: " + std::string(PQerrorMessage(conn)));
        res = PQgetResult(conn);
        if (res == NULL
                || PQresultStatus(res) == PGRES_BAD_RESPONSE
                || PQresultStatus(res) == PGRES_FATAL_ERROR)
            throw PgSqlException("Error executing statement: " + std::string(PQerrorMessage(conn)));

    }

    /**
     * Copy row into database. The statement has to be executed before this is called and it has to initiate copying.
     * @param args
     */
    template<typename... Args>
    void copy_data(Args... args)
    {
        ct.copy(*db, args...);
    }

    /**
     *
     * @return sql query used to initialize this Statement
     */
    std::string get_sql() const
    {
        return sql;
    }

private:
    Database* db;
    std::string name;
    std::string sql;
    bool prep;
    BindTypes bt;
    RetTypes rt;
    CopyTypes ct;
    PGparam* param;
    PGresult* res;
    bool cp;
};

}

#endif
