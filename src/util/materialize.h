/*
 * materialize.h
 *
 *  Created on: Feb 24, 2012
 *      Author: martin
 */

#ifndef MATERIALIZE_H_
#define MATERIALIZE_H_

#include <boost/range/concepts.hpp>
#include <vector>

namespace util
{

template<typename Rng>
auto materialize(Rng const& rng) -> std::vector<typename boost::range_value<Rng>::type>
{
	std::vector<typename boost::range_value<Rng>::type> ret;
	for(auto it = rng.begin(); it!= rng.end(); ++it)
		ret.push_back(*it);
	return ret;
}

}

#endif /* MATERIALIZE_H_ */
