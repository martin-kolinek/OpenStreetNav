#include <boost/test/unit_test.hpp>
#include "../xmlparse/xmlparse.h"
#include <iostream>

BOOST_AUTO_TEST_SUITE(xml)

BOOST_AUTO_TEST_CASE(node)
{
    std::string input(" <osm> <node id=\"1234556\" lat=\"34.252\" lon=\"21.512\"> <tag k=\"key\" v=\"val\" /> </node> </osm>");
    osmxml::XmlParser p;
    std::vector<osm::Node> correct;
    osm::Node n1(1234556, 34.252, 21.512);
    n1.tags.push_back(osm::Tag("key", "val"));
    correct.push_back(n1);
    std::vector<osm::Node> nodes;
    p.node_signal().connect([&nodes](osm::Node const& nd){nodes.push_back(nd);});
    p.parse_memory(input);
    BOOST_CHECK(correct == nodes);
}

BOOST_AUTO_TEST_SUITE_END()
