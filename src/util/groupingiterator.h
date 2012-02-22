/*
 * groupingiterator.h
 *
 *  Created on: Feb 21, 2012
 *      Author: martin
 */

#ifndef GROUPINGITERATOR_H_
#define GROUPINGITERATOR_H_

#include <boost/iterator/iterator_facade.hpp>
#include <boost/range/iterator_range.hpp>

namespace util
{

template<typename It, typename EqualityPred, typename Combinator, typename Accumulator>
class grouping_iterator : public boost::iterator_facade<grouping_iterator<It, EqualityPred, Combinator, Accumulator>, const Accumulator, boost::forward_traversal_tag>
{
private:
    It it;
    It end_it;
    Combinator comb;
    Accumulator accum_start, current;
    EqualityPred eql;
    bool done;
public:
    grouping_iterator(It const& start, It const& last, EqualityPred eq = EqualityPred(), Combinator comb = Combinator(), Accumulator accum_start = Accumulator()):
        it(start),
        end_it(last),
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
        return it == other.it && done == other.done;
    }
    void increment()
    {
        group();
    }
    void group()
    {
        if (it == end_it)
        {
            done = true;
            return;
        }
        current = accum_start;
        auto cur = *it;
        comb(current, cur);
        ++it;
        while (it != end_it && eql(cur, *it))
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

template<typename It, typename EqualityPred, typename Combinator, typename Accumulator>
grouping_iterator<It, EqualityPred, Combinator, Accumulator> make_grouping_iterator(It const& begin, It const& end, EqualityPred eq = EqualityPred(), Combinator comb = Combinator(), Accumulator accum_start = Accumulator())
{
    return grouping_iterator<It, EqualityPred, Combinator, Accumulator>(begin, end, eq, comb, accum_start);
}

template<typename Range, typename EqualityPred, typename Combinator, typename Accumulator>
class groupped_range : public boost::iterator_range<grouping_iterator<typename boost::range_iterator<Range>::type, EqualityPred, Combinator, Accumulator> >
{
private:
    typedef typename boost::range_iterator<Range>::type input_it_t;
    typedef grouping_iterator<input_it_t, EqualityPred, Combinator, Accumulator> grp_it_t;
    typedef boost::iterator_range<grp_it_t> base_t;
public:
    groupped_range(Range& rng, EqualityPred const& eq, Combinator const& comb, Accumulator const& acc):
        base_t(make_grouping_iterator(rng.begin(), rng.end(), eq, comb, acc),
               make_grouping_iterator(rng.end(), rng.end(), eq, comb, acc))
    {
    }
};

class groupped_trait
{
};

template<typename EqPred, typename Comb, typename Acc>
std::tuple<EqPred, Comb, Acc, groupped_trait> groupped(EqPred eq, Comb c, Acc ac)
{
    return std::make_tuple(eq, c, ac, groupped_trait());
}

template<typename Rng, typename EqPred, typename Comb, typename Acc>
groupped_range<Rng, EqPred, Comb, Acc> operator|(Rng& r, std::tuple<EqPred, Comb, Acc, groupped_trait> const& holder)
{
    return groupped_range<Rng, EqPred, Comb, Acc>(r, std::get<0>(holder), std::get<1>(holder), std::get<2>(holder));
}

} /* namespace util */
#endif /* GROUPINGITERATOR_H_ */
