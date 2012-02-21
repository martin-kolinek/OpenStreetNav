/*
 * sortedcombiterator.h
 *
 *  Created on: Feb 21, 2012
 *      Author: martin
 */

#ifndef SORTEDCOMBITERATOR_H_
#define SORTEDCOMBITERATOR_H_

#include "make_ref.h"

namespace util
{

template <typename It1, typename It2, typename Less, typename Combinator>
class sorted_comb_iterator : public boost::iterator_facade<sorted_comb_iterator, decltype(make_ref<Combinator>()(*make_ref<It1>(), *make_ref<It2>())), boost::forward_traversal_tag>
{
private:
	It1 start1;
	It1 const& end1;
	It2 start2;
	It2 const& end2;
	Less less;
	Combinator comb;
	typedef decltype(make_ref<Combinator>()(*make_ref<It1>(), *make_ref<It2>())) value_t;
	value_t current;
public:
	sorted_comb_iterator(It1 start1, It1 const& end1, It2 start2, It2 const& end2, Less less = Less(), Combinator comb = Combinator()):
		start1(start1),
		end1(end1),
		start2(start2),
		end2(end2),
		less(less),
		comb(comb)
	{
		fix();
	}
	void fix()
	{
		while(true)
		{
			if(less(start1, start2))
				++start1;
			else if(less(start2, start1))
				++start2;
			else
				break;
		}
		current = comb(*start1, *start2);
	}
	value_t const& dereference()
	{
		return current;
	}
	bool equal(sorted_comb_iterator const& other)
	{
		return start1==other.start1 && start2==other.start2 && end1==other.end1 && end2==other.end2;
	}
	void increment()
	{
		++start1;
		++start2;
		fix();
	}
	virtual ~sorted_comb_iterator()
	{
	}
};

} /* namespace util */
#endif /* SORTEDCOMBITERATOR_H_ */
