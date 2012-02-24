/*
 * tuple_sub_eq.h
 *
 *  Created on: Feb 23, 2012
 *      Author: martin
 */

#ifndef TUPLE_SUB_EQ_H_
#define TUPLE_SUB_EQ_H_

#include <tuple>

namespace util
{

template<size_t... elems>
class TupleEq
{

};

template<>
class TupleEq<>
{
public:
	template<typename Tup>
	bool operator()(Tup const&, Tup const&)
	{
		return true;
	}
};

template<size_t first, size_t... rest>
class TupleEq<first, rest...>
{
public:
	template<typename Tup>
	bool operator()(Tup const& a, Tup const& b)
	{
		return std::get<first>(a)==std::get<first>(b) && TupleEq<rest...>()(a, b);
	}
};

template<size_t... indices>
TupleEq<indices...> get_tuple_comparer()
{
	return TupleEq<indices...>();
}

}

#endif /* TUPLE_SUB_EQ_H_ */
