/*
 * ConcatCollection.h
 *
 *  Created on: Feb 3, 2012
 *      Author: martin
 */

#ifndef CONCATCOLLECTION_H_
#define CONCATCOLLECTION_H_

#include <boost/iterator/iterator_facade.hpp>

namespace util
{

template<typename It1, typename It2>
class ConcatCollection
{
private:
    It1 begin1, end1;
    It2 begin2, end2;
public:
    typedef decltype(*begin1) value_t;
    class iterator : public boost::iterator_facade<iterator, value_t, boost::random_access_traversal_tag>
    {
    private:
        It1 it1, end1;
        It2 it2, begin2;
    public:
        iterator(It1 it1, It1 end1, It2 it2, It2 begin2):
            it1(it1),
            end1(end1),
            it2(it2),
            begin2(begin2)
        {
        }
    private:
        friend class boost::iterator_core_access;
        void increment()
        {
            if (it1 == end1)
                ++it2;
            else
                ++it1;
        }

        bool equal(iterator const& other) const
        {
            return it1 == other.it1 && it2 == other.it2 && end1 == other.end1 && begin2 == other.begin2;
        }

        void decrement()
        {
            if (it2 == begin2)
                --it1;
            else
                --it2;
        }
        template<typename T>
        void advance(T const& number)
        {
            if (number < 0)
            {
                auto dist = std::distance(begin2, it2);
                if (-number < dist)
                    it2 += number;
                else
                {
                    it2 = begin2;
                    it1 += number + dist;
                }
            }
            else
            {
                auto dist = std::distance(it1, end1);
                if (number < dist)
                    it1 += number;
                else
                {
                    it1 = end1;
                    it2 += number - dist;
                }
            }
            auto dist1 = std::distance(it1, end1);
        }

        auto distance_to(iterator const& other) -> decltype(std::distance(it1, end1) + std::distance(begin2, it2))
        {
            return std::distance(it1, end1) + std::distance(begin2, it2);
        }

        auto dereference() const -> value_t
        {
            if (it1 != end1)
            return *it1;
            return *it2;
        }

    };
ConcatCollection(It1 begin1, It1 end1, It2 begin2, It2 end2):
        begin1(begin1),
        end1(end1),
        begin2(begin2),
        end2(end2)
    {
    }

    iterator begin()
    {
        return iterator(begin1, end1, begin2, begin2);
    }

    iterator end()
    {
        return iterator(end1, end1, end2, begin2);
    }

    virtual ~ConcatCollection()
    {
    }
};

template<typename It1, typename It2>
ConcatCollection<It1, It2> make_concat_coll(It1 begin1, It1 end1, It2 begin2, It2 end2)
{
    return ConcatCollection<It1, It2>(begin1, end1, begin2, end2);
}

} /* namespace pathfind */
#endif /* CONCATCOLLECTION_H_ */
