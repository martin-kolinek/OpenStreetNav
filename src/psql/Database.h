#ifndef PSQL_DATABASE_H_
#define PSQL_DATABASE_H_

#include <libpq-fe.h>
#include <string>

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
    PGconn* get_db();
    virtual ~Database();
private:
    PGconn* conn;
    bool async;
};

}

#endif
