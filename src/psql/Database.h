#ifndef PSQL_DATABASE_H_
#define PSQL_DATABASE_H_

#include <libpq-fe.h>
#include <string>
#include <vector>
#include <unordered_map>
#include <set>
#include <boost/signal.hpp>

namespace psql
{
class IStatement;

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
    PGconn* get_db();
    boost::signal<void (PGresult const&)>& notice_signal();
    virtual ~Database();
private:
    PGconn* conn;
    bool async;
    bool conn_synchr;
    std::unordered_map<std::string, IStatement*> stmts;
    boost::signal<void (PGresult const&)> notice_sig;
    std::vector<std::string> to_dealloc;
    std::set<std::string> savepoints;
    void receiveNotice(PGresult const* res);
    friend void noticeReceiver(void* arg, PGresult const* res);
};

}

#endif
