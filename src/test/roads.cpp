#include <boost/test/unit_test.hpp>
#include "../roads/roads.h"
#include <cmath>

BOOST_AUTO_TEST_SUITE(roads)

BOOST_AUTO_TEST_CASE(roadnet)
{
    RoadNetwork net;
    net.add_edge(RoadEdgeWithNodes(1, 0, 1, true, 1, 0, 1));
    net.add_edge(RoadEdgeWithNodes(2, 1, 2, true, 2, 1, 2));
    auto c = net.get_path_find_context(0, 2);
    auto p = c.get_start_node();
    std::vector<double> costs {0, 1, 0, 2};
    std::vector<RoadNetworkNode const*> v {p};
    for (int i = 0; i < 4; ++i)
    {
        BOOST_CHECK(p->neighbours.size() == 1);
        BOOST_CHECK(std::abs(p->neighbours[0].first - costs[i]) < 0.00001);
        p = p->neighbours[0].second;
        v.push_back(p);
    }
    BOOST_CHECK(p->neighbours.size() == 0);
    BOOST_CHECK(c.is_end_node(p));
    auto edges = net.resolve_path(v);
    BOOST_CHECK(edges.size() == 2);
    BOOST_CHECK(edges[0].way.id == 1);
    BOOST_CHECK(edges[1].way.id == 2);
    BOOST_CHECK(edges[0].start_seq_no == 0);
    BOOST_CHECK(edges[1].start_seq_no == 1);
}

BOOST_AUTO_TEST_CASE(roadnet2)
{
    RoadNetwork net;
    net.add_edge(RoadEdgeWithNodes(1, 0, 1, true, 1, 0, 1));
    net.add_edge(RoadEdgeWithNodes(1, 1, 2, true, 2, 0, 2));
    auto c = net.get_path_find_context(0, 2);
    auto p = c.get_start_node();
    BOOST_CHECK(p->neighbours.size() == 2);

}

BOOST_AUTO_TEST_SUITE_END()
