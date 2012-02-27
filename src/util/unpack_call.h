/*
 * unpack_call.h
 *
 *  Created on: Feb 5, 2012
 *      Author: martin
 */

#ifndef UNPACK_CALL_H_
#define UNPACK_CALL_H_

#include "seq.h"

namespace util
{

template<typename Func, typename... Args>
auto unpack_call(Func& f, std::tuple<Args...> const& arg_tuple) -> decltype(call_func(f, arg_tuple, typename gen<sizeof...(Args)>::type()))
{
    return call_func(f, arg_tuple, typename gen<sizeof...(Args)>::type());
}

template<typename Func, typename... Args, int... S>
auto call_func(Func& f, std::tuple<Args...> const& arg_tuple, seq<S...>) -> decltype(f(std::get<S>(arg_tuple)...))
{
    return f(std::get<S>(arg_tuple)...);
}

template<typename Func>
class uncurry_wrapper
{
private:

public:
    Func f;
    uncurry_wrapper(Func f):
        f(f)
    {}
    template<typename Tup>
    auto operator()(Tup const& arg) -> decltype(unpack_call(f, arg))
    {
        return unpack_call(f, arg);
    }
};

template<typename Func>
uncurry_wrapper<Func> uncurry(Func f)
{
    return uncurry_wrapper<Func>(f);
}

}

#endif /* UNPACK_CALL_H_ */
