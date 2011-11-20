#ifndef PGSQLEXECPTION_H_
#define PGSQLEXCEPTION_H_

#include <exception>
#include <string>

namespace psql
{

class PgSqlException : public std::exception
{
public:
    PgSqlException(std::string const& msg);
    virtual ~PgSqlException() throw();
    virtual const char* what() const throw();
private:
    std::string msg;
};

}

#endif
