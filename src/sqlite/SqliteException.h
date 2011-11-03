#include <exception>
#include <string>
#include <sqlite3.h>

namespace sqlite
{

class SqliteException : public std::exception
{
public:
    SqliteException(sqlite3* db);
    SqliteException(int code, sqlite3* db);
    SqliteException(int code, std::string const& msg);
    virtual ~SqliteException() throw();
    virtual const char* what() const throw();
private:
    std::string msg;
    int code;
};

void throw_sqlite_status(int code, sqlite3* db);

}
