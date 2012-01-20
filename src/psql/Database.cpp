#include "Database.h"

#include <poll.h>
#include <libpqtypes.h>
#include "psql.h"
#include "PgSqlException.h"

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
    cursors.clear();
}

void Database::rollback_transaction()
{
    savepoints.clear();
    execute_sql(*this, "ROLLBACK TRANSACTION");
    cursors.clear();
}

void Database::savepoint(const std::string& name)
{
    if (savepoints.count(name))
        execute_sql(*this, "RELEASE SAVEPOINT " + name);

    execute_sql(*this, "SAVEPOINT " + name);
    savepoints.insert(name);
}
void Database::rollback_to_savepoint(const std::string& name)
{
    execute_sql(*this, "ROLLBACK TO SAVEPOINT " + name);
}

void Database::analyze()
{
    execute_sql(*this, "ANALYZE");
}

void Database::set_schema(std::string schema)
{
    if (schema != "")
        schema += ",";
    execute_sql(*this, "SET search_path TO " + schema + "public");
}

void Database::create_schema(const std::string& schema)
{
    execute_sql(*this, "CREATE SCHEMA " + schema);
}

bool Database::in_transaction()
{
    auto c = get_db();
    auto res = PQtransactionStatus(c);
    return res == PQTRANS_INTRANS;
}

bool Database::in_failed_transaction()
{
    auto c = get_db();
    auto res = PQtransactionStatus(c);
    return res == PQTRANS_INERROR;
}

void Database::add_cursor(ICursor* curs)
{
    cursors.insert(curs);
}

void Database::remove_cursor(ICursor* curs)
{
    cursors.erase(curs);
}

bool Database::is_cursor(ICursor* curs) const
{
    return cursors.find(curs) != cursors.end();
}

void noticeReceiver(void* arg, const PGresult* res)
{
    ((Database*)arg)->receiveNotice(res);
}

Database::Database(const std::string& conninfo, bool synchr)
    : conn(NULL), conn_synchr(synchr)
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
        PQsetNoticeReceiver(conn, noticeReceiver, (void*)((((((this)))))));
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
    unsigned int dealloc_target = 0;
    while (to_dealloc.size() > dealloc_target)
    {
        std::string name(to_dealloc[dealloc_target]);
        auto res = PQexec(conn, ("DEALLOCATE " + name).c_str());
        if (res == NULL || PQresultStatus(res) != PGRES_COMMAND_OK)
        {
            dealloc_target++;
        }
        else
        {
            to_dealloc.erase(to_dealloc.begin() + dealloc_target);
        }
        if (res != NULL)
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
