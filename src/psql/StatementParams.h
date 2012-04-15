/*
 * StatementParams.h
 *
 *  Created on: Apr 15, 2012
 *      Author: martin
 */

#ifndef STATEMENTPARAMS_H_
#define STATEMENTPARAMS_H_

#include <postgres.h>
#include <vector>
#include <string>

namespace psql
{

class StatementParams
{
public:
    StatementParams(int n);
    StatementParams& operator=(StatementParams const& other);
    StatementParams& operator=(StatementParams && other);
    void set(int index, Oid o, std::string const& s);
    int nparams();
    const Oid* paramTypes();
    const char* const* paramValues();
    ~StatementParams();
private:
    int n;
    std::vector<char const*> vals2;
    std::vector<std::string> vals;
    std::vector<Oid> oids;
};

} /* namespace psql */
#endif /* STATEMENTPARAMS_H_ */
