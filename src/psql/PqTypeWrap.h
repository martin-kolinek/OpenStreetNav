/*
 * PqTypeWrap.h
 *
 *  Created on: Nov 22, 2011
 *      Author: martin
 */

#ifndef PQTYPEWRAP_H_
#define PQTYPEWRAP_H_

#include <libpq-fe.h>
#include <postgres_ext.h>
#include <postgres.h>
#include <catalog/pg_type.h>
#include <stdint.h>
#include <string>
#include <vector>
#include "../util/util.h"
#include "StatementParams.h"

namespace psql
{

void zero_get_check(int);
void zero_put_check(int);
void check_type(Oid, Oid);
void check_type(Oid, std::vector<Oid> const& oids);
void check_text(int);

template<typename T>
class PqTypeWrap
{
};

template<>
class PqTypeWrap<double>
{
public:
    double get(PGresult* res, int row, int col)
    {
        return util::parse<double>(PQgetvalue(res, row, col));
    }

    void check(PGresult* res, int col)
    {
        check_type(PQftype(res, col), FLOAT8OID);
        check_text(PQfformat(res, col));
    }

    void put(StatementParams& p, int index, double val)
    {
        p.set(index, FLOAT8OID, util::to_str(val));
    }
};

template<>
class PqTypeWrap<bool>
{
public:
    bool get(PGresult* res, int row, int col)
    {
        std::string s(PQgetvalue(res, row, col));
        return s == "t";
    }

    void check(PGresult* res, int col)
    {
        check_type(PQftype(res, col), BOOLOID);
        check_text(PQfformat(res, col));
    }

    void put(StatementParams& p, int index, bool val)
    {
        p.set(index, BOOLOID, util::to_str(val));
    }
};

template<>
class PqTypeWrap<int>
{
public:
    int get(PGresult* res, int row, int col)
    {
        return util::parse<int>(PQgetvalue(res, row, col));
    }

    void check(PGresult* res, int col)
    {
        check_type(PQftype(res, col), INT4OID);
        check_text(PQfformat(res, col));
    }

    void put(StatementParams& p, int index, int val)
    {
        p.set(index, INT4OID, util::to_str(val));
    }
};

template<>
class PqTypeWrap<int64_t>
{
public:
    int get(PGresult* res, int row, int col)
    {
        return util::parse<int64_t>(PQgetvalue(res, row, col));
    }

    void check(PGresult* res, int col)
    {
        check_type(PQftype(res, col), INT8OID);
        check_text(PQfformat(res, col));
    }

    void put(StatementParams& p, int index, int64_t val)
    {
        p.set(index, INT8OID, util::to_str(val));
    }
};

template<>
class PqTypeWrap<std::string>
{
public:
    std::string get(PGresult* res, int row, int col)
    {
        return std::string(PQgetvalue(res, row, col));
    }

    void check(PGresult* res, int col)
    {
        check_type(PQftype(res, col), std::vector<Oid> {TEXTOID, VARCHAROID});
        check_text(PQfformat(res, col));
    }

    void put(StatementParams& p, int index, std::string const& val)
    {
        p.set(index, TEXTOID, val);
    }
};

} /* namespace display */
#endif /* PQTYPEWRAP_H_ */
