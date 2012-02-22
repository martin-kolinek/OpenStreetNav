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

template <typename It1, typename It2, typename Combinator, typename Less>
class sorted_comb_iterator : public boost::iterator_facade<sorted_comb_iterator<It1, It2, Combinator, Less>, const decltype(make_ref<Combinator>()(*make_ref<It1>(), *make_ref<It2>())), boost::forward_traversal_tag>
{
public:
    It1 start1;
    It1 end1;
    It2 start2;
    It2 end2;
    Less less;
    Combinator comb;
    typedef decltype(make_ref<Combinator>()(*make_ref<It1>(), *make_ref<It2>())) value_t;
    value_t current;
public:
    sorted_comb_iterator(It1 const& start1, It1 const& end1, It2 const& start2, It2 const& end2, Combinator comb = Combinator(), Less less = Less()):
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
        while (true)
        {
            if (start1 != end1 && start2 != end2 && less(*start1, *start2))
                ++start1;
            else if (start1 != end1 && start2 != end2 && less(*start2, *start1))
                ++start2;
            else if (start1 == end1 || start2 == end2)
            {
                start1 = end1;
                start2 = end2;
                break;
            }
            else
                break;
        }

        if (start1 != end1 && start2 != end2)
        {
            current = comb(*start1, *start2);
        }
    }
    value_t const& dereference() const
    {
        return current;
    }
    bool equal(sorted_comb_iterator const& other) const
    {
        return start1 == other.start1 && start2 == other.start2 && end1 == other.end1 && end2 == other.end2;
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

template < typename It1, typename It2, typename Combinator, typename Less = std::less<decltype(*(make_ref<It1>()))> >
sorted_comb_iterator<It1, It2, Combinator, Less> make_sorted_combinator(It1 const& start1, It1 const& end1, It2 const& start2, It2 const& end2, Combinator comb = Combinator(), Less less = Less())
{
    auto s1 = start1;
    auto s2 = start2;
    auto e1 = end1;
    auto e2 = end2;
    return sorted_comb_iterator<It1, It2, Combinator, Less>(s1, e1, s2, e2, comb, less);
}

template<typename Rng1, typename Rng2, typename Combinator, typename Less>
class sorted_comb_range : public boost::iterator_range<sorted_comb_iterator<typename boost::range_iterator<Rng1>::type, typename boost::range_iterator<Rng2>::type, Combinator, Less> >
{
public:
    typedef typename boost::range_iterator<Rng1>::type it1_t;
    typedef typename boost::range_iterator<Rng2>::type it2_t;
    typedef sorted_comb_iterator<it1_t, it2_t, Combinator, Less> sorted_comb_it_t;
    typedef boost::iterator_range<sorted_comb_it_t> base_t;
public:
    sorted_comb_range(Rng1& rng1, Rng2& rng2, Combinator comb, Less less):
        base_t(make_sorted_combinator(rng1.begin(), rng1.end(), rng2.begin(), rng2.end(), comb, less),
               make_sorted_combinator(rng1.end(), rng1.end(), rng2.end(), rng2.end(), comb, less))
    {
    }
};

template < typename Rng1, typename Rng2, typename Combinator, typename Less = std::less<decltype(*make_ref<Rng1>().begin())> >
sorted_comb_range<Rng1, Rng2, Combinator, Less> sorted_combine(Rng1& rng1, Rng2& rng2, Combinator c = Combinator(), Less l = Less())
{
    return sorted_comb_range<Rng1, Rng2, Combinator, Less>(rng1, rng2, c, l);
}

} /* namespace util */
#endif /* SORTEDCOMBITERATOR_H_ */
