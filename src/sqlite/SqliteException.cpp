#include "SqliteException.h"
#include "InvalidUseException.h"
#include <sstream>

namespace sqlite
{

SqliteException::SqliteException(sqlite3* db):
    msg(sqlite3_errmsg(db)),
    code(sqlite3_errcode(db))
{
}

SqliteException::SqliteException(int code, std::string const& msg):
    msg(msg),
    code(code)
{
}

SqliteException::SqliteException(int code, sqlite3* db):
    msg(sqlite3_errmsg(db)),
    code(code)
{
}

SqliteException::~SqliteException() throw()
{
}

const char* SqliteException::what() const throw()
{
    std::ostringstream sstream;
    sstream << "Sqlite error, code: " << code << ", message: " << msg;
    return sstream.str().c_str();
}

void throw_sqlite_status(int code, sqlite3* db)
{
    if (code == SQLITE_OK)
        return;
    if (code == SQLITE_MISUSE)
        throw InvalidUseException("SQLite reports MISUSE");
    throw SqliteException(code, db);
}

}
