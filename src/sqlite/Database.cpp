/*
 * Database.cpp
 *
 *  Created on: Nov 2, 2011
 *      Author: martin
 */

#include "Database.h"
#include "SqliteException.h"
#include "InvalidUseException.h"

#include "Statement.h"

namespace sqlite
{

Database::Database(const std::string& filename)
    : conn(NULL)
{
    throw_sqlite_status(sqlite3_open(filename.c_str(), &conn), conn);
}

Database::~Database()
{
    force_close();
}

Database& Database::operator =(Database && other)
{
    force_close();
    conn = other.conn;
    other.conn = NULL;
    stmts = other.stmts;
    other.stmts.clear();
    return *this;
}

Database::Database(Database && other)
    : conn(NULL)
{
    *this = std::move(other);
}

void Database::close()
{
    if (stmts.size() > 0)
        throw InvalidUseException("Trying to close connection with non-finalized statements");
    if (conn == NULL)
        return;
    sqlite3_close(conn);
    conn = NULL;
}

void Database::force_close() throw()
{
    while (stmts.size() > 0)
        (*stmts.begin())->finalize();
    close();
}

void Database::register_statement(Statement& st)
{
    if (stmts.count(&st))
        return;
    stmts.insert(&st);
}

void Database::unregister_statement(Statement& st)
{
    stmts.erase(&st);
}

int Database::unfinalized()
{
    return stmts.size();
}

sqlite3* const& Database::cobj()
{
    return conn;
}

}
