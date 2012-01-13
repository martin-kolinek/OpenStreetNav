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

class Database
{
public:
    Database(Database const&) = delete;
    Database& operator=(Database const&) = delete;
    Database(Database && other);
    Database& operator=(Database && other);
    Database(std::string const& conninfo, bool synchr = false);
    void regist(std::string const& name, std::string const& sql, IStatement* st);
    void unregist(std::string const& name, IStatement* st);
    void begin_transaction();
    void commit_transaction();
    void rollback_transaction();
    void savepoint(std::string const& name);
    void rollback_to_savepoint(std::string const& name);
    void analyze();
    void set_schema(std::string const& schema);
    void create_schema(std::string const& schema);
    bool in_transaction();
    bool in_failed_transaction();
    PGconn* get_db();
    boost::signal<void (PGresult const&)>& notice_signal();
    void add_cursor(ICursor* curs);
    void remove_cursor(ICursor* curs);
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
