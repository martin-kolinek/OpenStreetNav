/*
 * CopyTypes.h
 *
 *  Created on: Jan 7, 2012
 *      Author: martin
 */

#ifndef COPYTYPES_H_
#define COPYTYPES_H_

#include <string>
#include "Database.h"
#include "PgSqlException.h"
#include "../util/util.h"

namespace psql
{

namespace cptypes
{

template<typename... Args>
class sanitize
{
};

template<>
class sanitize<>
{
public:
    void operator()()
    {
    }
};

template<typename Head, typename... Tail>
class sanitize<Head, Tail...>
{
public:
    void operator()(Head&, Tail& ... t)
    {
        sanitize<Tail...>()(t...);
    }
};

template<typename... Tail>
class sanitize<std::string, Tail...>
{
public:
    void operator()(std::string& s, Tail& ... t)
    {
        std::map<char, std::string> m
        {
            std::make_pair('\t', "\\t"),
            std::make_pair('\r', "\\r"),
            std::make_pair('\n', "\\n"),
            std::make_pair('\\', "\\\\")
        };
        s = util::replace(s, m);
        sanitize<Tail...>()(t...);
    }
};

}

template<typename... Args>
class CopyTypes
{
public:
    CopyTypes(std::string const& row_sep = "\n", std::string const& col_sep = "\t"):
        row_sep(row_sep),
        col_sep(col_sep)
    {
    }
    void copy(Database& db, Args... a)
    {
        cptypes::sanitize<Args...>()(a...);
        auto s = util::concatenate("\t", a...) + "\n";
        auto conn = db.get_db();
        auto result = PQputCopyData(conn, s.c_str(), s.size());
        if (result == 0)
            throw PgSqlException("Sorry copy for asynchronous connections is not implemented");
        if (result == -1)
            throw PgSqlException("Error sending copy data: " + std::string(PQerrorMessage(conn)));
    }
protected:
    std::string row_sep, col_sep;
};

} /* namespace psql*/
#endif /* COPYTYPES_H_ */
