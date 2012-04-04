#include <boost/test/unit_test.hpp>
#include "../elements/osmelements.h"
#include <boost/property_tree/xml_parser.hpp>

BOOST_AUTO_TEST_SUITE(osmelements_test)

BOOST_AUTO_TEST_CASE(description)
{
    osm::Node n(1, 2, 3);
    n.tags.insert(osm::Tag("nkey", "nval"));
    auto ndesc = n.get_description();
    BOOST_CHECK(ndesc.get<std::string>("node") == "1");
    BOOST_CHECK(ndesc.get<std::string>("node.tags.nkey") == "nval");
    osm::Way w(1);
    w.add_node(osm::Node(2));
    w.tags.insert(osm::Tag("wkey", "wval"));
    auto wdesc = w.get_description();
    BOOST_CHECK(wdesc.get<std::string>("way") == "1");
    BOOST_CHECK(wdesc.get<std::string>("way.nodes.node") == "2");
    BOOST_CHECK(wdesc.get<std::string>("way.tags.wkey") == "wval");
    osm::Relation r(1);
    r.tags.insert(osm::Tag("rkey", "rval"));
    r.add_node("role", osm::Node(2));
    auto rdesc = r.get_description();
    BOOST_CHECK(rdesc.get<std::string>("relation") == "1");
    BOOST_CHECK(rdesc.get<std::string>("relation.members.member.node") == "2");
    BOOST_CHECK(rdesc.get<std::string>("relation.members.member.role") == "role");
    BOOST_CHECK(rdesc.get<std::string>("relation.tags.rkey") == "rval");
}

BOOST_AUTO_TEST_CASE(equals)
{
    osm::Node n(1, 2, 3);
    n.tags.insert(osm::Tag("asdf", "bsdf"));
    n.tags.insert(osm::Tag("asdf", "csdf"));
    osm::Node n2(1, 2, 3);
    n2.tags.insert(osm::Tag("asdf", "csdf"));
    n2.tags.insert(osm::Tag("asdf", "bsdf"));
    BOOST_CHECK(n == n2);
    osm::Way w(1);
    w.add_node(osm::Node(2));
    w.add_node(osm::Node(3));
    w.tags.insert(osm::Tag("asdf", "bsdf"));
    w.tags.insert(osm::Tag("asdf", "csdf"));
    osm::Way w2(1);
    w2.add_node(osm::Node(2));
    w2.add_node(osm::Node(3));
    w2.tags.insert(osm::Tag("asdf", "csdf"));
    w2.tags.insert(osm::Tag("asdf", "bsdf"));
    BOOST_CHECK(w == w2);
    w2 = osm::Way(1);
    w2.add_node(osm::Node(3));
    w2.add_node(osm::Node(2));
    w2.tags.insert(osm::Tag("asdf", "csdf"));
    w2.tags.insert(osm::Tag("asdf", "bsdf"));
    BOOST_CHECK(w != w2);
    osm::Relation r(1);
    r.add_node("role", osm::Node(2));
    r.add_way("role", osm::Way(3));
    r.tags.insert(osm::Tag("asdf", "bsdf"));
    r.tags.insert(osm::Tag("asdf", "csdf"));
    osm::Relation r2(1);
    r2.add_way("role", osm::Way(3));
    r2.add_node("role", osm::Node(2));
    r2.tags.insert(osm::Tag("asdf", "csdf"));
    r2.tags.insert(osm::Tag("asdf", "bsdf"));
    BOOST_CHECK(r == r2);

    osm::Node n3(1, 2, 3);
    n3.tags.insert(osm::Tag("asdf", "bsdf"));
    n3.tags.insert(osm::Tag("asdf", "csdf"));
    osm::Node n4(1, 2, 3);
    n4.tags.insert(osm::Tag("asdf", "csdf"));
    n4.tags.insert(osm::Tag("bsdf", "bsdf"));
    BOOST_CHECK(n3 != n4);
}

BOOST_AUTO_TEST_CASE(wayregion)
{
    osm::Way w(1);
    osm::Node n(1);
    for (int i = 0; i < 10; ++i)
    {
        n.id = i;
        w.add_node(n);
    }
    osm::WayRegion wr(w, std::vector<std::pair<unsigned int, unsigned int> > {std::make_pair(2, 4), std::make_pair(7, 8)});
    osm::Edge e1(osm::Node(0), 0, osm::Node(1), 1, osm::Way(1));
    osm::Edge e2(osm::Node(0), 0, osm::Node(1), 1, osm::Way(3));
    osm::Edge e3(osm::Node(2), 2, osm::Node(3), 3, osm::Way(1));
    osm::Edge e4(osm::Node(2), 2, osm::Node(4), 4, osm::Way(1));
    osm::Edge e5(osm::Node(1), 1, osm::Node(5), 5, osm::Way(1));
    osm::Edge e6(osm::Node(5), 5, osm::Node(6), 6, osm::Way(1));
    osm::Edge e7(osm::Node(7), 7, osm::Node(9), 9, osm::Way(1));
    BOOST_CHECK(!wr.intersects(e1));
    BOOST_CHECK(!wr.intersects(e2));
    BOOST_CHECK(wr.intersects(e3));
    BOOST_CHECK(wr.intersects(e4));
    BOOST_CHECK(wr.intersects(e5));
    BOOST_CHECK(!wr.intersects(e6));
    BOOST_CHECK(wr.intersects(e7));
}

BOOST_AUTO_TEST_SUITE_END()
