#ifndef PSQL_STATEMENT_H_
#define PSQL_STATEMENT_H_

#include "Database.h"
#include "PgSqlException.h"
#include "CopyTypes.h"
#include "BindTypes.h"
#include <libpqtypes.h>
#include <tuple>
#include "../util.h"

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
    Statement():
        db(NULL),
        param(NULL),
        res(NULL),
        cp(false)
    {
    }

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
    Statement(Statement && other):
        db(NULL),
        param(NULL),
        res(NULL)
    {
        *this = std::move(other);
    }

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

    typename RetTypes::RowType get_row(int row)
    {
        if (res == NULL)
            throw PgSqlException("get_values called with no result");
        return rt.get_values(res, row);
    }

    int row_count()
    {
        if (res == NULL)
            return 0;
        if (PQresultStatus(res) == PGRES_TUPLES_OK)
            return PQntuples(res);
        return 0;
    }

    int affected_rows()
    {
        if (res == NULL)
            return 0;
        std::string aff(PQcmdTuples(res));
        if (aff == "")
            return 0;
        return util::parse<int>(aff);
    }

    bool copying()
    {
        return cp;
    }

    void end_copy()
    {
        auto conn = db->get_db();
        cp = false;
        auto result = PQputCopyEnd(conn, NULL);
        if (result == 0)
            throw PgSqlException("Sorry copy for asynchronous connections is not implemented");
        if (result == -1)
            throw PgSqlException("Error sending end copy request: " + std::string(PQerrorMessage(conn)));
    }

    template<typename... Args>
    void copy_data(Args... args)
    {
        ct.copy(*db, args...);
    }

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
