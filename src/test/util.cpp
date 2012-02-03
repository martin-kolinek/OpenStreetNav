#include <boost/test/unit_test.hpp>

#include "../util/util.h"
#include "../util/ConcatCollection.h"
#include <algorithm>

BOOST_AUTO_TEST_SUITE(util)

BOOST_AUTO_TEST_CASE(concat)
{
    BOOST_CHECK_EQUAL("1 2 3", concatenate(" ", 1, 2, 3));
}

BOOST_AUTO_TEST_CASE(mult_eq)
{
    std::multimap<int, int> mp;
    mp.insert(std::pair<int, int>(1, 2));
    mp.insert(std::pair<int, int>(1, 3));

    std::multimap<int, int> mp2;
    mp2.insert(std::pair<int, int>(1, 3));
    mp2.insert(std::pair<int, int>(1, 2));
    multimap_eq(mp, mp2);
}

BOOST_AUTO_TEST_CASE(concat_it)
{
    std::vector<int> v1 {1, 2, 3, 4};
    std::vector<int> v2 {5, 6, 7, 8};
    auto coll = util::make_concat_coll(v1.begin(), v1.end(), v2.begin(), v2.end());
    std::vector<int> v {1, 2, 3, 4, 5, 6, 7, 8};
    BOOST_CHECK(std::equal(coll.begin(), coll.end(), v.begin()));
}

BOOST_AUTO_TEST_SUITE_END()
