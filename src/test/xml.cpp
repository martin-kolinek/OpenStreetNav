#include <boost/test/unit_test.hpp>
#include "../xmlparse/xmlparse.h"
#include <iostream>
#include "../util/util.h"

BOOST_AUTO_TEST_SUITE(xml)

BOOST_AUTO_TEST_CASE(basic)
{
    std::string input(" <osm version=\"adsf\"> <node id=\"1234556\" lat=\"34.252\" lon=\"21.512\"> <tag k=\"key\" v=\"val\" /> </node>");
    input += "<node id=\"35312\" lat=\"234\" lon=\"23\" />";
    input += "<way id=\"5432\"><nd ref=\"555\" /><tag k=\"wkey\" v=\"wval\" /> </way>";
    input += "<relation id=\"1253\"> <member type=\"node\" ref=\"2521\" role=\"role\" />";
    input += "<member type=\"way\" ref=\"2522\" role=\"role\" />";
    input += "<member type=\"relation\" ref=\"2523\" role=\"role\" /> <tag k=\"rkey\" v=\"rval\" /> </relation></osm>";
    osmxml::XmlParser p;
    std::vector<osm::Node> correctn;
    osm::Node n1(1234556, 34.252, 21.512);
    n1.tags.insert(osm::Tag("key", "val"));
    correctn.push_back(n1);
    n1 = osm::Node(35312, 234, 23);
    correctn.push_back(n1);
    std::vector<osm::Way> correctw;
    osm::Way w1(5432);
    w1.tags.insert(osm::Tag("wkey", "wval"));
    w1.add_node(555);
    correctw.push_back(w1);
    std::vector<osm::Relation> correctr;
    osm::Relation r1(1253);
    r1.tags.insert(osm::Tag("rkey", "rval"));
    r1.add_node("role", osm::Node(2521));
    r1.add_way("role", osm::Way(2522));
    r1.add_rel("role", osm::Relation(2523));
    correctr.push_back(r1);
    std::vector<osm::Node> nodes;
    std::vector<osm::Way> ways;
    std::vector<osm::Relation> rels;
    p.node_handler = [&nodes](osm::Node const & nd)
    {
        nodes.push_back(nd);
    };
    p.way_handler = [&ways](osm::Way const & w)
    {
        ways.push_back(w);
    };
    p.relation_handler = [&rels](osm::Relation const & r)
    {
        rels.push_back(r);
    };
    try
    {
        p.parse_memory(input);
        BOOST_CHECK(correctn == nodes);
        BOOST_CHECK(correctw == ways);
        BOOST_CHECK(correctr == rels);
    }
    catch (xml_schema::parsing& e)
    {
        BOOST_CHECK_MESSAGE(false, util::to_str(e));
    }

}

BOOST_AUTO_TEST_CASE(isthrowing)
{
    osmxml::XmlParser p;
    try
    {
        p.parse_memory("<assdf></asdf>");
    }
    catch (std::exception& ex)
    {
        return;
    }
    BOOST_ERROR("Expected parsing exception");

}

BOOST_AUTO_TEST_SUITE_END()
