#ifndef PGSQLEXCEPTION_H_
#define PGSQLEXCEPTION_H_

#include <exception>
#include <string>

namespace psql
{

/**
 * \class PgSqlException
 * Exceptions of this type are thrown from inside the psql namespace to indicate problem with database interface.
 */
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
