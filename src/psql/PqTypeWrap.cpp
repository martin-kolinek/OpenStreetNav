/*
 * PqTypeWrap.cpp
 *
 *  Created on: Nov 22, 2011
 *      Author: martin
 */

#include "PqTypeWrap.h"
#include "PgSqlException.h"
#include <boost/range/adaptors.hpp>
#include "../util/range.h"

namespace psql
{

void check_type(Oid a, Oid b)
{
    if (a != b)
    {
        throw PgSqlException(util::concatenate(" ", "Type mismatch in db retrieval, expected", a, "got", b));
    }
}

void check_type(Oid oid, std::vector<Oid> const& oids)
{
    util::any(oids | boost::adaptors::filtered([oid](Oid a)
    {
        return a == oid;
    }));
}

void check_text(int i)
{
    if (i != 0)
        throw PgSqlException("Sorry, binary data retrieval not supported");
}

} /* namespace display */
