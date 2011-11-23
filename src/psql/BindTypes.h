/*
 * BindTypes.h
 *
 *  Created on: Nov 22, 2011
 *      Author: martin
 */

#ifndef BINDTYPES_H_
#define BINDTYPES_H_

#include "PqTypeWrap.h"

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
    void put_prot(PGparam* param, Head h, Tail... t)
    {
        tw.put(param, h);
        BindTypes<Tail...>::put_prot(param, t...);
    }
public:
    void put(PGparam* param, Head h, Tail... t)
    {
        PQparamReset(param);
        put_prot(param, h, t...);
    }
};

template<>
class BindTypes<>
{
protected:
    void put_prot(PGparam*)
    {
    }
public:
    void put(PGparam* param)
    {
        PQparamReset(param);
    }
};

} /* namespace psql */
#endif /* BINDTYPES_H_ */
