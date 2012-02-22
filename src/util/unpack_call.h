/*
 * unpack_call.h
 *
 *  Created on: Feb 5, 2012
 *      Author: martin
 */

#ifndef UNPACK_CALL_H_
#define UNPACK_CALL_H_

namespace util
{

template<int... S>
class seq
{
};

template<int N, int... S>
class gen : public gen < N - 1, N - 1, S... >
{

};

template<int... S>
class gen<0, S...>
{
public:
    typedef seq<S...> type;
};

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

}

#endif /* UNPACK_CALL_H_ */
