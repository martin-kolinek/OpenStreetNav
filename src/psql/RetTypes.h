/*
 * RetTypes.h
 *
 *  Created on: Nov 22, 2011
 *      Author: martin
 */

#ifndef RETTYPES_H_
#define RETTYPES_H_

#include <tuple>
#include <libpqtypes.h>
#include "PqTypeWrap.h"

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
    std::tuple<Head, Tail...> get_values(PGresult* res, int row, int col)
    {
        return std::tuple_cat(tw.get(res, row, col), RetTypes<Tail...>::get_values(res, row, col + 1));
    }
public:
    typedef std::tuple<Head, Tail...> RowType;

    RowType get_values(PGresult* res, int row)
    {
        return get_values(res, row, 0);
    }
};

template<>
class RetTypes<>
{
protected:
    std::tuple<> get_value(PGresult*, int, int)
    {
        return std::tuple<>();
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
