/*
 * wayreduction_test.cpp
 *
 *  Created on: Jan 12, 2012
 *      Author: martin
 */

#include <boost/test/unit_test.hpp>
#include "../wayreduction/wayreduction.h"
#include "../util/util.h"

BOOST_AUTO_TEST_SUITE(wayreduction)

bool eq_node_pair(std::pair<int, osm::Node> const& p1, std::pair<int, osm::Node> const& p2)
{
    return p1.first == p2.first && p1.second.id == p2.second.id;
}

BOOST_AUTO_TEST_CASE(waynodefilt)
{
    wayred::WayNodeFilter filt;
    filt.add_important("key", "val");
    filt.add_important("asdf", "bsdf");
    osm::Way w(1);
    w.add_node(osm::Node(2, 0.4, 0.4));
    w.add_node(osm::Node(3, 0.4, 0.8));
    w.add_node(osm::Node(4, 0.4, 0.8));
    w.tags.insert(osm::Tag("key", "val"));
    osm::Way w2(2);
    w2.add_node(3);
    w2.add_node(5);
    w2.tags.insert(osm::Tag("asdf", "bsdf"));
    w2.tags.insert(osm::Tag("fcda", "gas"));
    osm::Way w3(3);
    w3.tags.insert(osm::Tag("key", "val"));
    w3.add_node(osm::Node(4, 0.4, 0.8));
    w3.add_node(osm::Node(5, 0.4, 0.8));
    w3.add_node(osm::Node(6, 0.4, 0.8));
    w3.add_node(osm::Node(7, 0.4, 0.8));
    osm::Way w4(4);
    w4.add_node(5);
    w4.add_node(7);
    osm::Way w5(5);
    w5.add_node(6);
    w5.add_node(8);
    std::vector<std::pair<osm::Way, std::multimap<osm::Node, osm::Way, osm::LtByID> > > exp;
    std::multimap<osm::Node, osm::Way, osm::LtByID> mp;
    mp.insert(std::make_pair(osm::Node(3, 0.4, 0.8), w2));
    mp.insert(std::make_pair(osm::Node(4, 0.4, 0.8), w3));
    exp.push_back(std::make_pair(w, mp));
    mp.clear();
    mp.insert(std::make_pair(osm::Node(4, 0.4, 0.8), w));
    mp.insert(std::make_pair(osm::Node(5, 0.4, 0.8), w4));
    mp.insert(std::make_pair(osm::Node(5, 0.4, 0.8), w2));
    mp.insert(std::make_pair(osm::Node(6, 0.4, 0.8), w5));
    exp.push_back(std::make_pair(w3, mp));
    auto it = w3.nodes.begin();
    std::advance(it, 2);
    w3.nodes.erase(it);
    auto ret = filt.process_range(exp);
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
            return util::equal_collection(w1.nodes, w2.nodes, eq_node_pair);
        }
    };
    BOOST_CHECK(util::equal_collection<Comp>(ret, expected_ret));
}

BOOST_AUTO_TEST_SUITE_END()
