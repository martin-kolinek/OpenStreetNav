/*
 * RetTypes.h
 *
 *  Created on: Nov 22, 2011
 *      Author: martin
 */

#ifndef RETTYPES_H_
#define RETTYPES_H_

#include <tuple>
#include "PqTypeWrap.h"
#include "PgSqlException.h"

namespace psql
{

template<typename... Tail>
class RetTypes
{};

template<typename Head, typename... Tail>
class RetTypes<Head, Tail...> : private RetTypes<Tail...>
{
private:
    PqTypeWrap<Head> tw;
protected:
    std::tuple<Head, Tail...> get_values_prot(PGresult* res, int row, int col)
    {
        return std::tuple_cat(std::make_tuple(tw.get(res, row, col)), RetTypes<Tail...>::get_values_prot(res, row, col + 1));
    }

    void check_prot(PGresult* res, int col)
    {
        tw.check(res, col);
        RetTypes<Tail...>::check_prot(col + 1);
    }
public:
    typedef std::tuple<Head, Tail...> RowType;

    RowType get_values(PGresult* res, int row)
    {
        return get_values_prot(res, row, 0);
    }

    void check(PGresult* res)
    {
        return check_prot(res, 0);
    }
};

template<>
class RetTypes<>
{
protected:
    std::tuple<> get_values_prot(PGresult*, int, int)
    {
        return std::tuple<>();
    }
    void check_prot(PGresult* res, int col)
    {
        if (PQnfields(res) != col)
            throw PgSqlException("Statement not retrieving all data in result");
    }
public:
    typedef std::tuple<> RowType;

    RowType get_values(PGresult*, int)
    {
        return std::tuple<>();
    }
};

} /* namespace psql */
#endif /* RETTYPES_H_ */
