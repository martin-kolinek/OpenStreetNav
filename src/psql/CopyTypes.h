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

namespace psql
{

template<typename... Args>
class CopyTypes
{
};

template<>
class CopyTypes<>
{
public:
    CopyTypes(std::string const& row_sep = "\n", std::string const& col_sep = "\t"):
        row_sep(row_sep),
        col_sep(col_sep)
    {
    }
    virtual void copy(Database&)
    {
    }
protected:
    std::string row_sep, col_sep;
};

template<typename Head>
class CopyTypes<Head> : protected CopyTypes<>
{
public:
    CopyTypes(std::string const& row_sep = "\n", std::string const& col_sep = "\t"):
        CopyTypes<>(row_sep, col_sep)
    {
    }
    void copy(Database& db, Head h)
    {
        std::ostringstream str;
        add_to_ostream(str, h);
        auto conn = db.get_db();
        auto result = PQputCopyData(conn, str.str().c_str(), str.str().size());
        if (result == 0)
            throw PgSqlException("Sorry copy for asynchronous connections is not implemented");
        if (result == -1)
            throw PgSqlException("Error sending copy data: " + std::string(PQerrorMessage(conn)));
    }
protected:
    void add_to_ostream(std::ostream& ost, Head val)
    {
        ost << val << CopyTypes<>::row_sep;
    }
};

template<typename Head, typename Head2, typename... Tail>
class CopyTypes<Head, Head2, Tail...> : protected CopyTypes<Head2, Tail...>
{
public:
    CopyTypes(std::string const& row_sep = "\n", std::string const& col_sep = "\t"):
        CopyTypes<Head2, Tail...>(row_sep, col_sep)
    {
    }
    void copy(Database& db, Head h, Head2 h2, Tail... t)
    {
        std::ostringstream str;
        add_to_ostream(str, h, h2, t...);
        auto conn = db.get_db();
        auto s(str.str());
        auto result = PQputCopyData(conn, s.c_str(), s.size());
        if (result == 0)
            throw PgSqlException("Sorry copy for asynchronous connections is not implemented");
        if (result == -1)
            throw PgSqlException("Error sending copy data: " + std::string(PQerrorMessage(conn)));
    }
protected:
    void add_to_ostream(std::ostream& ost, Head h, Head2 h2, Tail... t)
    {
        ost << h << CopyTypes<>::col_sep;
        CopyTypes<Head2, Tail...>::add_to_ostream(ost, h2, t...);
    }
};

} /* namespace psql*/
#endif /* COPYTYPES_H_ */
