/*
 * osmdb.cpp
 *
 *  Created on: Nov 6, 2011
 *      Author: martin
 */

#include <boost/test/unit_test.hpp>
#include <boost/filesystem.hpp>
#include "../osmdb/osmdb.h"

class OsmDBFixture
{
public:
    OsmDBFixture():
        pdb("")
    {

        psql::execute_sql(pdb, "CREATE SCHEMA testing");
        psql::execute_sql(pdb, "SET search_path TO testing, public");
    }
    ~OsmDBFixture()
    {
        psql::execute_sql(pdb, "DROP SCHEMA testing CASCADE");
    }
    psql::Database pdb;
};

BOOST_FIXTURE_TEST_SUITE(OsmDBCreateTests, OsmDBFixture)

BOOST_AUTO_TEST_CASE(create)
{
    osmdb::OsmDatabase db(pdb);
    db.create_tables();
}

BOOST_AUTO_TEST_CASE(indexes)
{
    osmdb::OsmDatabase db(pdb);
    db.create_tables();
    db.create_indexes();
}

BOOST_AUTO_TEST_CASE(insert)
{

    osmdb::OsmDatabase db(pdb);
    db.create_tables();
    db.create_indexes();
    osmdb::ElementInsertion ins(db);
    osm::Node n(123, 1, 2);
    n.tags.push_back(osm::Tag("nkey", "nval"));
    ins.insert_node(n);
    n.tags.clear();
    n.id = 124;
    ins.insert_node(n);
    osm::Way w(341);
    w.nodes.push_back(123);
    w.nodes.push_back(124);
    w.tags.push_back(osm::Tag("wkey", "wval"));
    ins.insert_way(w);
    std::vector<std::tuple<int64_t, double, double> > nodes {std::make_tuple(123, 2, 1), std::make_tuple(124, 2, 1)};
    std::vector<std::tuple<int64_t> > ways {std::make_tuple(341)};
    std::vector<std::tuple<int64_t, int64_t, int64_t> > edges {std::make_tuple(341, 123, 124)};
    std::vector<std::tuple<int64_t, int64_t, int> > waynodes
    {
        std::make_tuple(341, 123, 0),
        std::make_tuple(341, 124, 1)
    };
    std::sort(waynodes.begin(), waynodes.end());
    std::vector<std::tuple<int64_t, std::string, std::string> > ndattrs {std::make_tuple(123, "nkey", "nval")};
    std::vector<std::tuple<int64_t, std::string, std::string> > wattrs {std::make_tuple(341, "wkey", "wval")};
    auto nodes2 = psql::query_sql<int64_t, double, double>(db.get_db(), "SELECT ID, ST_X(Location::geometry), ST_Y(Location::geometry) FROM Nodes");
    auto ways2 = psql::query_sql<int64_t>(db.get_db(), "SELECT ID FROM Ways");
    auto waynodes2 = psql::query_sql<int64_t, int64_t, int>(db.get_db(), "SELECT WayID, NodeID, SequenceNo FROM WayNodes");
    auto edges2 = psql::query_sql<int64_t, int64_t, int64_t>(db.get_db(), "SELECT WayID, StartNodeID, EndNodeID FROM Edges");
    std::sort(waynodes2.begin(), waynodes2.end());
    auto ndattrs2 = psql::query_sql<int64_t, std::string, std::string>(db.get_db(), "SELECT NodeID, Key, Value FROM NodeAttributes");
    auto wattrs2 = psql::query_sql<int64_t, std::string, std::string>(db.get_db(), "SELECT WayID, Key, Value FROM WayAttributes");
    BOOST_CHECK(nodes == nodes2);
    BOOST_CHECK(ways == ways2);
    BOOST_CHECK(edges == edges2);
    BOOST_CHECK(waynodes == waynodes2);
    BOOST_CHECK(ndattrs == ndattrs2);
    BOOST_CHECK(wattrs == wattrs2);
}

BOOST_AUTO_TEST_CASE(empty_displaydb)
{
    osmdb::OsmDatabase odb(pdb);
    odb.create_tables();
    odb.create_indexes();
    osmdb::DisplayDB db(odb);
    db.set_bounds(geo::Point(0, 0), geo::Point(1, 1), 1);
}

BOOST_AUTO_TEST_CASE(simple)
{
    osmdb::OsmDatabase odb(pdb);
    odb.create_tables();
    odb.create_indexes();
    osmdb::DisplayDB db(odb);
    osmdb::ElementInsertion ins(db.get_db());
    osm::Node nd(1, 0.5, 0.5);
    nd.tags.push_back(osm::Tag("disp", "disp"));
    ins.insert_node(nd);
    ins.insert_node(osm::Node(2, 0.4, 0.4));
    ins.insert_node(osm::Node(3, 0.4, 0.8));
    osm::Way w(1);
    w.nodes.push_back(2);
    w.nodes.push_back(3);
    w.tags.push_back(osm::Tag("disp", "disp"));
    ins.insert_way(w);
    nd.id = 4;
    nd.lat = 1.5;
    nd.lon = 0.5;
    ins.insert_node(nd);
    ins.insert_node(osm::Node(5, 0.6, 0.4));
    db.set_to_show("disp", "disp", 0, 15);
    db.set_bounds(geo::Point(1, 0), geo::Point(0, 1), 1);
    BOOST_CHECK(db.get_points().size() == 1);
    BOOST_CHECK(db.get_edges().size() == 1);
}

BOOST_AUTO_TEST_SUITE_END()
