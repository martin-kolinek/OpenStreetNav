/*
 * BindTypes.h
 *
 *  Created on: Nov 22, 2011
 *      Author: martin
 */

#ifndef BINDTYPES_H_
#define BINDTYPES_H_

#include "PqTypeWrap.h"
#include "StatementParams.h"

namespace psql
{

template<typename... Args>
class BindTypes
{
};

template<typename Head, typename... Tail>
class BindTypes<Head, Tail...> : private BindTypes<Tail...>
{
private:
    PqTypeWrap<Head> tw;
protected:
    void put(StatementParams& p, int index, Head h, Tail... t)
    {
        tw.put(p, index, h);
        BindTypes<Tail...>::put(p, index + 1, t...);
    }
public:
    StatementParams get_params(Head h, Tail... t)
    {
        StatementParams ret(sizeof...(t) + 1);
        put(ret, 0, h, t...);
        return ret;
    }
};

template<>
class BindTypes<>
{
protected:
    void put(StatementParams&, int)
    {
    }
public:
    StatementParams get_params()
    {
        return StatementParams(0);
    }
};

} /* namespace psql */
#endif /* BINDTYPES_H_ */
