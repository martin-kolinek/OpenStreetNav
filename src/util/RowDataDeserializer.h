/*
 * RowDataDeserializer.h
 *
 *  Created on: Feb 4, 2012
 *      Author: martin
 */

#ifndef ROWDATADESERIALIZER_H_
#define ROWDATADESERIALIZER_H_

#include <tuple>
#include "../util/unpack_call.h"
#include <bitset>

namespace util
{

namespace row_data_deserializer
{
template<typename RowType, typename HandlersType, int max, int i>
class FireUnfired
{
public:
	static void fire(RowType& last_vals, HandlersType& handlers, std::bitset<std::tuple_size<RowType>::value >& fired)
	{
		if(!fired[max-i])
		{
			fired[max-i]=true;
			FireUnfired<RowType, HandlersType, max, i-1>::fire(last_vals, handlers, fired);
			util::unpack_call(std::get<max-i>(handlers), last_vals);
		}
	}
};

template<typename RowType, typename HandlersType, int max>
class FireUnfired<RowType, HandlersType, max, 0>
{
public:
	static void fire(RowType&, HandlersType&, std::bitset<std::tuple_size<RowType>::value >&)
	{
	}
};


template<typename RowType, typename HandlersType, int i>
class Process
{
public:
	static int process(RowType const& tup, RowType& last_vals, HandlersType& handlers, std::bitset<std::tuple_size<RowType>::value > initialized, std::bitset<std::tuple_size<RowType>::value >& fired, bool last = false)
	{
		int changed = 0;
		if(!initialized[i] || last || std::get<i>(tup)!=std::get<i>(last_vals))
		{
			if(initialized[i])
			{
				FireUnfired<RowType, HandlersType, std::tuple_size<RowType>::value, std::tuple_size<RowType>::value-i >::fire(last_vals, handlers, fired);
			}
			std::get<i>(last_vals)=std::get<i>(tup);
			initialized[i]=true;
			changed = i;
		}
		return std::max(changed, Process<RowType, HandlersType, i-1>::process(tup, last_vals, handlers, initialized, fired, last));
	}
};

template<typename RowType, typename HandlersType>
class Process<RowType, HandlersType, -1>
{
public:
	static int process(RowType const&, RowType&, HandlersType&, std::bitset<std::tuple_size<RowType>::value >, std::bitset<std::tuple_size<RowType>::value >&, bool)
	{
		return -1;
	}
};

}

template<typename RowType, typename... ColHandlers>
class RowDataDeserializer
{
private:
	std::tuple<ColHandlers...> handlers;
	RowType last_vals;
	std::bitset<std::tuple_size<RowType>::value > initialized;
	std::bitset<std::tuple_size<RowType>::value > fired;

	template<size_t i>
	void fire_unfired()
	{
		if(i==std::tuple_size<RowType>::value)
			return;

	}

public:
	RowDataDeserializer(ColHandlers... handlers):
		handlers(handlers...)
	{

	}

	template<typename It>
	It work(It begin, It const& end, bool finish)
	{
		It last_top=begin;
		for(; begin!=end; ++begin)
		{
			if(row_data_deserializer::Process<RowType, std::tuple<ColHandlers...>, std::tuple_size<RowType>::value - 1>::process(*begin, last_vals, handlers, initialized, fired) == 0)
				last_top=begin;
		}
		if(finish)
		{
			row_data_deserializer::Process<RowType, std::tuple<ColHandlers...>, std::tuple_size<RowType>::value - 1>::process(last_vals, last_vals, handlers, initialized, fired, true);
			return end;
		}
		return last_top;
	}

	virtual ~RowDataDeserializer()
	{
	}
};

template<typename It, typename RowType, typename... ColHandlers>
It deserialize_collection2(It begin, It const& end, bool finish, RowType const&, ColHandlers const&... handlers)
{
	RowDataDeserializer<RowType, ColHandlers...> des(handlers...);
	return des.work(begin, end, finish);
}

template<typename It, typename... ColHandlers>
It deserialize_collection(It begin, It const& end, bool finish, ColHandlers const&... handlers)
{
	return deserialize_collection2(begin, end, finish, *begin, handlers...);
}

} /* namespace util */
#endif /* ROWDATADESERIALIZER_H_ */
