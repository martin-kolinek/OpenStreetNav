/*
 * wayreduction_test.cpp
 *
 *  Created on: Jan 12, 2012
 *      Author: martin
 */

#include <boost/test/unit_test.hpp>
#include "../wayreduction/wayreduction.h"
#include "../util.h"

BOOST_AUTO_TEST_SUITE(wayreduction)

BOOST_AUTO_TEST_CASE(waynodefilt)
{
    wayred::WayNodeFilter filt;
    filt.add_important("key", "val");
    filt.add_important("asdf", "bsdf");
    osm::Way w(1);
    w.nodes.push_back(osm::Node(2, 0.4, 0.4));
    w.nodes.push_back(osm::Node(3, 0.4, 0.8));
    w.nodes.push_back(osm::Node(4, 0.4, 0.8));
    w.tags.insert(osm::Tag("key", "val"));
    osm::Way w2(2);
    w2.nodes.push_back(3);
    w2.nodes.push_back(5);
    w2.tags.insert(osm::Tag("asdf", "bsdf"));
    w2.tags.insert(osm::Tag("fcda", "gas"));
    osm::Way w3(3);
    w3.tags.insert(osm::Tag("key", "val"));
    w3.nodes.push_back(osm::Node(4, 0.4, 0.8));
    w3.nodes.push_back(osm::Node(5, 0.4, 0.8));
    w3.nodes.push_back(osm::Node(6, 0.4, 0.8));
    w3.nodes.push_back(osm::Node(7, 0.4, 0.8));
    osm::Way w4(4);
    w4.nodes.push_back(5);
    w4.nodes.push_back(7);
    osm::Way w5(5);
    w5.nodes.push_back(6);
    w5.nodes.push_back(8);
    std::map<osm::Way, std::multimap<osm::Node, osm::Way, osm::LtByID>, osm::LtByID> exp;
    std::multimap<osm::Node, osm::Way, osm::LtByID> mp;
    mp.insert(std::make_pair(osm::Node(3, 0.4, 0.8), w2));
    mp.insert(std::make_pair(osm::Node(4, 0.4, 0.8), w3));
    exp[w] = mp;
    mp.clear();
    mp.insert(std::make_pair(osm::Node(4, 0.4, 0.8), w));
    mp.insert(std::make_pair(osm::Node(5, 0.4, 0.8), w4));
    mp.insert(std::make_pair(osm::Node(5, 0.4, 0.8), w2));
    mp.insert(std::make_pair(osm::Node(6, 0.4, 0.8), w5));
    exp[w3] = mp;
    w3.nodes.erase(w3.nodes.begin() + 2);
    auto ret = filt.reduce_ways(exp);
    std::vector<osm::Way> expected_ret
    {
        w,
        w3
    };
    class Comp
    {
    public:
        bool operator()(osm::Way const& w1, osm::Way const& w2)
        {
            if (w1.id != w2.id)
                return false;
            return util::equal_collection<osm::EqByID>(w1.nodes, w2.nodes);
        }
    };
    BOOST_CHECK(util::equal_collection<Comp>(ret, expected_ret));
}

BOOST_AUTO_TEST_SUITE_END()
