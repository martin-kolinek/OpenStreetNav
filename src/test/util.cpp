#include <boost/test/unit_test.hpp>

#include "../util.h"

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

BOOST_AUTO_TEST_SUITE_END()
