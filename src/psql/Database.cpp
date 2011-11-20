#include "Database.h"

#include <poll.h>
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
    return conn;
}

Database::~Database()
{
    if (conn != NULL)
        PQfinish(conn);
}
}
