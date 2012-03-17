/*
 * cost.cpp
 *
 *  Created on: Mar 5, 2012
 *      Author: martin
 */

#include <boost/test/unit_test.hpp>
#include "../util/range.h"
#include "../elements/osmelements.h"
#include "../cost/LengthAssigner.h"

BOOST_AUTO_TEST_SUITE(cost)

BOOST_AUTO_TEST_CASE(length1)
{
    osm::Node n1(1, 0, 1);
    osm::Node n2(2, 0, 2);
    osm::Node n3(3, 0, 3);
    osm::Node n4(4, 0, 4);
    osm::Node n5(5, 0, 5);
    osm::Node n6(6, 0, 6);
    osm::Way full(1);
    full.tags.insert(osm::Tag("highway", "primary"));
    full.nodes = std::map<int, osm::Node> {std::make_pair(0, n1), std::make_pair(1, n2), std::make_pair(2, n3), std::make_pair(3, n4), std::make_pair(4, n5), std::make_pair(5, n6)};
    osm::Way reduced(1);
    reduced.nodes = {std::make_pair(0, n1), std::make_pair(3, n4)};
    cost::LengthAssigner ln;
    auto v = ln.extract_edges(reduced, full);
    BOOST_CHECK(v.size() == 2);
    BOOST_CHECK(v[0].cost > 300);
    BOOST_CHECK(v[1].cost > 300);
    BOOST_CHECK(v[0].forward != v[1].forward);

    full.tags.insert(osm::Tag("oneway", "yes"));
    v = ln.extract_edges(reduced, full);
    BOOST_CHECK(v.size() == 1);
    BOOST_CHECK(v[0].forward);

    full.tags.erase(osm::Tag("oneway", "yes"));
    full.tags.insert(osm::Tag("oneway", "reverse"));
    v = ln.extract_edges(reduced, full);
    BOOST_CHECK(v.size() == 1);
    BOOST_CHECK(!v[0].forward);
}

BOOST_AUTO_TEST_SUITE_END()
