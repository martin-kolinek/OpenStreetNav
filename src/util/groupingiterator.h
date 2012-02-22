/*
 * groupingiterator.h
 *
 *  Created on: Feb 21, 2012
 *      Author: martin
 */

#ifndef GROUPINGITERATOR_H_
#define GROUPINGITERATOR_H_

#include <boost/iterator/iterator_facade.hpp>

namespace util
{

template<typename It, typename EqualityPred, typename Combinator, typename Accumulator>
class grouping_iterator : public boost::iterator_facade<grouping_iterator<It, EqualityPred, Combinator, Accumulator>, const Accumulator, boost::forward_traversal_tag>
{
private:
    It itt;
    It const& end_it;
    Combinator comb;
    Accumulator accum_start, current;
    EqualityPred eql;
    bool done;
public:
    grouping_iterator(It start, It const& end, EqualityPred eq = EqualityPred(), Combinator comb = Combinator(), Accumulator accum_start = Accumulator()):
        itt(start),
        end_it(end),
        comb(comb),
        accum_start(accum_start),
        current(accum_start),
        eql(eq),
        done(false)
    {
        group();
    }
    Accumulator const& dereference() const
    {
        return current;
    }
    bool equal(grouping_iterator const& other) const
    {
        return itt == other.itt && done == other.done;
    }
    void increment()
    {
        group();
    }
    void group()
    {
        if (itt == end_it)
        {
            done = true;
            return;
        }
        current = accum_start;
        auto cur = *itt;
        comb(current, cur);
        ++itt;
        while (itt != end_it && eql(cur, *itt))
        {
            cur = *itt;
            comb(current, cur);
            ++itt;
        }
    }
    virtual ~grouping_iterator()
    {
    }
};

template<typename It, typename EqualityPred, typename Combinator, typename Accumulator>
grouping_iterator<It, EqualityPred, Combinator, Accumulator> make_grouping_iterator(It begin, It end, EqualityPred eq = EqualityPred(), Combinator comb = Combinator(), Accumulator accum_start = Accumulator())
{
    return grouping_iterator<It, EqualityPred, Combinator, Accumulator>(begin, end, eq, comb, accum_start);
}

} /* namespace util */
#endif /* GROUPINGITERATOR_H_ */
