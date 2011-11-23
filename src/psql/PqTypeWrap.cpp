/*
 * PqTypeWrap.cpp
 *
 *  Created on: Nov 22, 2011
 *      Author: martin
 */

#include "PqTypeWrap.h"
#include "PgSqlException.h"

namespace psql
{

void zero_get_check(int i)
{
    if (i == 0)
        throw PgSqlException("Error extracting column value " + std::string(PQgeterror()));
}

void zero_put_check(int i)
{
    if (i == 0)
        throw PgSqlException("Error storing parameter value " + std::string(PQgeterror()));
}

} /* namespace display */
