#include "Database.h"

#include <poll.h>
#include <libpqtypes.h>
#include "PgSqlException.h"
#include "psql.h"

namespace psql
{

void Database::begin_transaction()
{
    execute_sql(*this, "BEGIN TRANSACTION");
}

void Database::commit_transaction()
{
    savepoints.clear();
    execute_sql(*this, "COMMIT TRANSACTION");
}

void Database::rollback_transaction()
{
    savepoints.clear();
    execute_sql(*this, "ROLLBACK TRANSACTION");
}

void Database::savepoint(std::string const& name)
{
    if (savepoints.count(name))
        execute_sql(*this, "RELEASE SAVEPOINT " + name);
    execute_sql(*this, "SAVEPOINT " + name);
    savepoints.insert(name);
}
void Database::rollback_to_savepoint(std::string const& name)
{
    execute_sql(*this, "ROLLBACK TO SAVEPOINT " + name);
}

void noticeReceiver(void* arg, const PGresult* res)
{
    ((Database*)(arg))->receiveNotice(res);
}

Database::Database(const std::string& conninfo, bool synchr)
    : conn(NULL)
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

    if (!async)
    {
        PQsetNoticeReceiver(conn, noticeReceiver, (void*)(this));
    }
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
            throw PgSqlException("Error initializing libpqtypes: " + std::string(PQgeterror()));
        PQsetNoticeReceiver(conn, noticeReceiver, (void*)this);
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

void Database::regist(const std::string& name, const std::string& sql, IStatement* st)
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

void Database::unregist(const std::string& name, IStatement* st)
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
boost::signal<void(const PGresult& )> & Database::notice_signal()
{
    return notice_sig;
}

void Database::receiveNotice(const PGresult* res)
{
    notice_sig(*res);
}

}
