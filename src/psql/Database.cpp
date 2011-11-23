#include "Database.h"

#include <poll.h>
#include <libpqtypes.h>
#include "PgSqlException.h"

namespace psql
{
Database::Database(std::string const& conninfo, bool synchr):
    conn(NULL)
{
    if (synchr)
    {
        conn = PQconnectdb(conninfo.c_str());
    }
    else
    {
        conn = PQconnectStart(conninfo.c_str());
    }
    if (conn == NULL)
        throw PgSqlException("Unable to allocate memory for connection object");
    async = !synchr;
    if (PQstatus(conn) == CONNECTION_BAD)
        throw PgSqlException("Unable to connect to postgresql server: " + std::string(PQerrorMessage(conn)));
    if (!async && PQinitTypes(conn) == 0)
        throw PgSqlException("Error initializing libpqtypes: " + std::string(PQgeterror()));

}

PGconn* Database::get_db()
{
    if (async)
    {
        int sock = PQsocket(conn);
        struct pollfd fd;
        fd.fd = sock;
        fd.events = POLLIN | POLLOUT;
        int status = PGRES_POLLING_WRITING;
        while (status != PGRES_POLLING_FAILED && status != PGRES_POLLING_OK)
        {
            poll(&fd, 1, -1);
            status = PQconnectPoll(conn);
        }
        if (PQstatus(conn) == CONNECTION_BAD)
            throw PgSqlException("Unable to connect to postgresql server: " + std::string(PQerrorMessage(conn)));
        async = false;
        if (PQinitTypes(conn) == 0)
            PgSqlException("Error initializing libpqtypes: " + std::string(PQgeterror()));
    }
    else
    {
        if (PQstatus(conn) == CONNECTION_BAD)
        {
            PQreset(conn);
            if (PQstatus(conn) == CONNECTION_BAD)
                throw PgSqlException("Connection to server lost: " + std::string(PQerrorMessage(conn)));
        }
    }
    while (to_dealloc.size() > 0)
    {
        std::string name(to_dealloc.back());
        to_dealloc.pop_back();
        auto res = PQexec(conn, ("DEALLOCATE " + name).c_str());
        if (res == NULL)
        {
            throw PgSqlException("Error deallocating prepared statement which had to be deallocated " + std::string(PQerrorMessage(conn)));
        }
        if (PQresultStatus(res) != PGRES_COMMAND_OK)
        {
            std::string str(PQresultErrorMessage(res));
            PQclear(res);
            throw PgSqlException("Error deallocating prepared statement which had to be deallocated " + str);
        }
        PQclear(res);
    }
    return conn;
}

Database::~Database()
{
    if (conn != NULL)
        PQfinish(conn);
}

void Database::regist(std::string const& name, std::string const& sql, IStatement* st)
{
    auto res = PQprepare(get_db(), name.c_str(), sql.c_str(), 0, NULL);
    if (res == NULL)
        throw PgSqlException("Error when preparing statement " + std::string(PQerrorMessage(conn)));
    if (PQresultStatus(res) != PGRES_COMMAND_OK)
    {
        std::string str(PQresultErrorMessage(res));
        PQclear(res);
        throw PgSqlException("Error when preparing statement " + str);
    }
    PQclear(res);
    stmts[name] = st;
}

void Database::unregist(std::string const& name, IStatement* st)
{
    if (!stmts.count(name))
        throw PgSqlException("Unregistering not registered statement");
    if (stmts[name] != st)
        return;
    auto res = PQexec(conn, ("DEALLOCATE " + name).c_str());
    if (res == NULL || PQresultStatus(res) != PGRES_COMMAND_OK)
    {
        if (res != NULL)
            PQclear(res);
        to_dealloc.push_back(name);
    }
}

}
