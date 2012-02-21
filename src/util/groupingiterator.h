/*
 * groupingiterator.h
 *
 *  Created on: Feb 21, 2012
 *      Author: martin
 */

#ifndef GROUPINGITERATOR_H_
#define GROUPINGITERATOR_H_

namespace util
{

template<typename It, typename EqualityPred, typename Combinator, typename Accumulator>
class grouping_iterator : public boost::iterator_facade<grouping_iterator, Accumulator, boost::forward_traversal_tag>
{
private:
	It it;
	It const& end;
	Combinator comb;
	Accumulator accum_start, current;
	EqualityPred eq;
public:
	grouping_iterator(It start, It const& end, EqualityPred eq = EqualityPred(), Combinator comb = Combinator(), Accumulator accum_start = Accumulator()):
		comb(comb),
		accum_start(accum_start),
		current(accum_start),
		eq(eq)
	{
		group();
	}
	Accumulator const& dereference()
	{
		return current;
	}
	bool equal(grouping_iterator const& other)
	{
		return it == other.it;
	}
	void increment()
	{
		group();
	}
	void group()
	{
		current=accum_start;
		auto cur = *it;
		comb(current, cur);
		++it;
		while(it!=end && eq(cur, *it))
		{
			cur = *it;
			comb(current, cur);
			++it;
		}
	}
	virtual ~grouping_iterator()
	{
	}
};

} /* namespace util */
#endif /* GROUPINGITERATOR_H_ */
