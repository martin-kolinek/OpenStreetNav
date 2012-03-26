#ifndef PSQL_DATABASE_H_
#define PSQL_DATABASE_H_

#include <libpq-fe.h>
#include <string>
#include <vector>
#include <unordered_map>
#include <unordered_set>
#include <set>
#include <boost/signal.hpp>

namespace psql
{
class IStatement;
class ICursor;

/**
 * \class Database
 * Represents a connection to PostgreSQL database
 */
class Database
{
public:
    Database(Database const&) = delete;
    Database& operator=(Database const&) = delete;
    /**
     * Move constructs Database from other Database
     * @param other
     */
    Database(Database && other);
    /**
     * Move assigns from other Database
     * @param other
     * @return *this
     */
    Database& operator=(Database && other);
    /**
     * Connects to database
     * @param conninfo connection info string (see libpq documentation)
     * @param synchr whether connection is synchronous
     */
    Database(std::string const& conninfo, bool synchr = false);
    /**
     * Prepares server side prepared statement.
     * psql::Statement class does this automatically so you probably shouldn't call this.
     * @param name statement name
     * @param sql statement sql
     * @param st pointer to statement
     */
    void regist(std::string const& name, std::string const& sql, IStatement* st);
    /**
     * Deallocates server side prepared statement.
     * psql::Statement class does this automatically so you probably shouldn't call this.
     * @param name statement name
     * @param st pointer to statement
     */
    void unregist(std::string const& name, IStatement* st);
    /**
     * Starts a transaction on this connection.
     */
    void begin_transaction();
    /**
     * Commits a transaction on this connection.
     */
    void commit_transaction();
    /**
     * Rollbacks a transaction on this connection.
     */
    void rollback_transaction();
    /**
     * Creates a savepoint in transaction.
     * @param name savepoint name
     */
    void savepoint(std::string const& name);
    /**
     * Rollbacks current transaction to given savepoint.
     * @param name savepoint name
     */
    void rollback_to_savepoint(std::string const& name);
    /**
     * Runs ANALYZE on database.
     */
    void analyze();

    std::string get_schema();
    /**
     * Sets search path on this connection.
     * @param schema search path (can be single schema or comma separated search path)
     */
    void set_schema(std::string schema);
    /**
     * Create schema with given name.
     * @param schema name of the schema to create
     */
    void create_schema(std::string const& schema);
    /**
     *
     * @return whether a non-failed transaction is in progress
     */
    bool in_transaction();
    /**
     *
     * @return wheteher a failed transaction is in progress (and thus rollback needs to be called before anything else)
     */
    bool in_failed_transaction();
    /**
     * Retrieve underlying PGconn* from this connection
     * @return underlying PGconn*
     */
    PGconn* get_db();
    /**
     *
     * @return boost::signal to connect to if you want to be informed about notices received from this connection
     */
    boost::signal<void (PGresult const&)>& notice_signal();
    /**
     * Adds a cursor to cursor collection.
     * Cursor class does this automatically, so you shouldn't need to call this.
     * @param curs
     */
    void add_cursor(ICursor* curs);
    /**
     * Removes a cursor from cursor collection.
     * Cursor class does this automatically, so you shouldn't need to call this.
     * @param curs
     */
    void remove_cursor(ICursor* curs);
    /**
     *
     * @param curs
     * @return true if curs is in cursor collection
     */
    bool is_cursor(ICursor* curs) const;
    virtual ~Database();
private:
    PGconn* conn;
    bool async;
    bool conn_synchr;
    std::unordered_map<std::string, IStatement*> stmts;
    std::unordered_set<ICursor*> cursors;
    boost::signal<void (PGresult const&)> notice_sig;
    std::vector<std::string> to_dealloc;
    std::set<std::string> savepoints;
    void receiveNotice(PGresult const* res);
    friend void noticeReceiver(void* arg, PGresult const* res);
};

}

#endif
