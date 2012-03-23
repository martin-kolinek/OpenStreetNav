/*
 * tuple_sub_eq.h
 *
 *  Created on: Feb 23, 2012
 *      Author: martin
 */

#ifndef TUPLE_SUB_EQ_H_
#define TUPLE_SUB_EQ_H_

#include <tuple>
#include "seq.h"

namespace util
{

template<size_t... indices, typename Tup>
auto sub_tie(Tup& t) -> decltype(std::tie(std::get<indices>(t)...))
{
    return std::tie(std::get<indices>(t)...);
}

template<size_t... indices, typename Tup>
auto sub_tuple(Tup const& t) -> decltype(std::tie(std::get<indices>(t)...))
{
    return std::make_tuple(std::get<indices>(t)...);
}

template<typename... Args>
std::tuple<Args const& ...> const_tie(Args const& ... args)
{
    return std::tuple<Args const & ...>(args...);
}

template<size_t... indices, typename Tup>
auto sub_const_tie(Tup const& t) -> decltype(const_tie(std::get<indices>(t)...))
{
    return const_tie(std::get<indices>(t)...);
}

template<template<typename T> class Func, size_t... elems>
class TupleFunc
{
public:
    typedef typename Func<int>::result_type result_type;
    template<typename Tup>
    bool operator()(Tup const& t1, Tup const& t2) const
    {
        return Func<decltype(sub_const_tie<elems...>(t1))>()(sub_const_tie<elems...>(t1), sub_const_tie<elems...>(t2));
    }
};

template<size_t... indices>
TupleFunc<std::equal_to, indices...> get_tuple_comparer()
{
    return TupleFunc<std::equal_to, indices...>();
}

template<size_t... indices>
TupleFunc<std::less, indices...> get_tuple_less()
{
    return TupleFunc<std::less, indices...>();
}

template<typename... Args>
auto tup_tail(std::tuple<Args...> const& tup) -> decltype(tail_impl(tup, gen_seq < sizeof...(Args) - 1 > ()))
{
    return tail_impl(tup, gen_seq < sizeof...(Args) - 1 > ());
}

template<typename Tup, int... S>
auto tail_impl(Tup& t, seq<S...>) -> decltype(sub_const_tie<S...>(t))
{
    return sub_const_tie < (S + 1)... > (t);
}

}

#endif /* TUPLE_SUB_EQ_H_ */
