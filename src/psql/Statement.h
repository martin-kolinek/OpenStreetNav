#ifndef PSQL_STATEMENT_H_
#define PSQL_STATEMENT_H_

#include "Database.h"
#include "PgSqlException.h"
#include <libpqtypes.h>
#include <tuple>
#include "../util.h"

namespace psql
{

class IStatement
{
};

template<typename BindTypes, typename RetTypes>
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
        res(NULL)
    {
    }

    Statement(std::string const& sql, Database& db):
        db(&db),
        sql(sql),
        prep(false),
        param(PQparamCreate(db.get_db())),
        res(NULL)
    {
        check_param();
    }
    Statement(std::string const& name, std::string const& sql, Database& db):
        db(&db),
        name(name),
        prep(true),
        param(PQparamCreate(db.get_db())),
        res(NULL)
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
        PQparamClear(param);
    }

    Statement& operator=(Statement const&) = delete;
    Statement(Statement<BindTypes, RetTypes> const&) = delete;
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
            res = PQparamExecPrepared(db->get_db(), param, name.c_str(), 1);
        else
            res = PQparamExec(db->get_db(), param, sql.c_str(), 1);
        if (res == NULL)
            throw PgSqlException("Error retrieving statement result: " + std::string(PQgeterror()));
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

private:
    Database* db;
    std::string name;
    std::string sql;
    bool prep;
    BindTypes bt;
    RetTypes rt;
    PGparam* param;
    PGresult* res;
};

}

#endif
