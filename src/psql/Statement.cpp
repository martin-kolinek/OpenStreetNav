#include "Statement.h"

namespace psql
{

template<>
PGresult* execBT<BindTypes<> >(PGconn* conn, StatementParams&, std::string const& sql)
{
    auto res = PQexec(conn, sql.c_str());
    if (res == NULL
            || PQresultStatus(res) == PGRES_BAD_RESPONSE
            || PQresultStatus(res) == PGRES_FATAL_ERROR)
        throw PgSqlException("Error executing statement: " + std::string(PQerrorMessage(conn)));
    return res;
}

template<>
PGresult* execPrepBT<BindTypes<> >(PGconn* conn, StatementParams&, std::string const& name)
{
    auto res = PQexecPrepared(conn, name.c_str(), 0, NULL, NULL, NULL, 1);
    if (res == NULL
            || PQresultStatus(res) == PGRES_BAD_RESPONSE
            || PQresultStatus(res) == PGRES_FATAL_ERROR)
        throw PgSqlException("Error executing statement: " + std::string(PQerrorMessage(conn)));
    return res;
}

}
