/*
 * StatementParams.cpp
 *
 *  Created on: Apr 15, 2012
 *      Author: martin
 */

#include "StatementParams.h"
#include "../util/range.h"
#include "../util/materialize.h"

namespace psql
{

StatementParams::StatementParams(int n):
    n(n),
    vals(n),
    oids(n)
{
}

int StatementParams::nparams()
{
    return n;
}

const Oid* StatementParams::paramTypes()
{
    return &oids[0];
}

void StatementParams::set(int index, Oid o, std::string const& s)
{
    oids[index] = o;
    vals[index] = s;
}

const char* const* StatementParams::paramValues()
{
    vals2 = util::materialize(vals | util::selected([](std::string const & s)
    {
        return s.c_str();
    }));
    return &vals2[0];
}

StatementParams::~StatementParams()
{
}

} /* namespace psql */
