/*
 * PqTypeWrap.h
 *
 *  Created on: Nov 22, 2011
 *      Author: martin
 */

#ifndef PQTYPEWRAP_H_
#define PQTYPEWRAP_H_

#include <libpqtypes.h>
#include <stdint.h>
#include <string>
#include <vector>

namespace psql
{

void zero_get_check(int);
void zero_put_check(int);

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
        PGfloat8 d;
        zero_get_check(PQgetf(res, row, "%float8", col, &d));
        return d;
    }

    void put(PGparam* param, double val)
    {
        zero_put_check(PQputf(param, "%float8", val));
    }
};

template<>
class PqTypeWrap<int>
{
public:
    int get(PGresult* res, int row, int col)
    {
        PGint4 i;
        zero_get_check(PQgetf(res, row, "%int4", col, &i));
        return i;
    }
    void put(PGparam* param, int val)
    {
        zero_put_check(PQputf(param, "%int4", val));
    }
};

template<>
class PqTypeWrap<int64_t>
{
public:
    int64_t get(PGresult* res, int row, int col)
    {
        PGint8 i;
        zero_get_check(PQgetf(res, row, "%int8", col, &i));
        return i;
    }
    void put(PGparam* param, int64_t val)
    {
        zero_put_check(PQputf(param, "%int8", val));
    }
};

template<>
class PqTypeWrap<std::vector<char> >
{
public:
    std::vector<char> get(PGresult* res, int row, int col)
    {
        PGbytea ret;
        zero_get_check(PQgetf(res, row, "%bytea", col, &ret));
        std::vector<char> v(ret.len);
        for (int i = 0; i < ret.len; ++i)
        {
            v[i] = ret.data[i];
        }
        return v;
    }
    void put(PGparam* param, std::vector<char>& vect)
    {
        PGbytea bytea;
        bytea.len = vect.size();
        bytea.data = &vect[0];
        zero_put_check(PQputf(param, "%bytea", &bytea));
    }
};

template<>
class PqTypeWrap<std::string>
{
public:
    std::string get(PGresult* res, int row, int col)
    {
        PGtext txt;
        zero_get_check(PQgetf(res, row, "%text", col, &txt));
        return std::string(txt);
    }
    void put(PGparam* param, std::string& str)
    {
        zero_put_check(PQputf(param, "%text", str.c_str()));
    }
};

} /* namespace display */
#endif /* PQTYPEWRAP_H_ */