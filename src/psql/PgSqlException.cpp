#include "PgSqlException.h"

namespace psql
{

PgSqlException::PgSqlException(std::string const& msg):
    msg("PostgreSQL problem: " + msg)
{
}

PgSqlException::~PgSqlException() throw()
{
}

char const* PgSqlException::what() const throw()
{
    return msg.c_str();
}

}
