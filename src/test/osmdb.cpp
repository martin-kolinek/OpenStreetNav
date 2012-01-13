/*
 * osmdb.cpp
 *
 *  Created on: Nov 6, 2011
 *      Author: martin
 */

#include <boost/test/unit_test.hpp>
#include <boost/filesystem.hpp>
#include <boost/property_tree/xml_parser.hpp>
#include "../osmdb/osmdb.h"
#include "../sqllib/sqllib.h"
#include <test_config.h>

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
    	if(pdb.in_transaction() || pdb.in_failed_transaction())
    		pdb.rollback_transaction();
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
    db.create_indexes_and_keys();
    db.drop_indexes_and_keys();
    db.create_indexes_and_keys();
    db.drop_indexes_and_keys();
}

BOOST_AUTO_TEST_CASE(insert)
{

    osmdb::OsmDatabase db(pdb);
    db.create_tables();
    db.create_indexes_and_keys();
    osmdb::ElementInsertion ins(db);
    osm::Node n(123, 1, 2);
    n.tags.insert(osm::Tag("nkey", "nval"));
    ins.insert_node(n);
    n.tags.clear();
    n.id = 124;
    ins.insert_node(n);
    osm::Way w(341);
    w.nodes.push_back(123);
    w.nodes.push_back(124);
    w.tags.insert(osm::Tag("wkey", "wval"));
    ins.insert_way(w);
    osm::Relation r(432);
    r.tags.insert(osm::Tag("rkey", "rval"));
    ins.insert_relation(r);
    r.tags.clear();
    r.id = 433;
    r.add_node("rnode", osm::Node(124));
    r.add_way("rway", osm::Way(341));
    r.add_rel("rrel", osm::Relation(432));
    ins.insert_relation(r);
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
    std::vector<std::tuple<int64_t> > rels {std::make_tuple(432), std::make_tuple(433)};
    std::vector<std::tuple<int64_t, std::string, std::string> > rattrs {std::make_tuple(432, "rkey", "rval")};
    std::vector<std::tuple<int64_t, std::string, int64_t> > nmembers {std::make_tuple(433, "rnode", 124)};
    std::vector<std::tuple<int64_t, std::string, int64_t> > wmembers {std::make_tuple(433, "rway", 341)};
    std::vector<std::tuple<int64_t, std::string, int64_t> > rmembers {std::make_tuple(433, "rrel", 432)};
    auto nodes2 = psql::query_sql<int64_t, double, double>(db.get_db(), "SELECT ID, ST_X(Location::geometry), ST_Y(Location::geometry) FROM Nodes");
    auto ways2 = psql::query_sql<int64_t>(db.get_db(), "SELECT ID FROM Ways");
    auto waynodes2 = psql::query_sql<int64_t, int64_t, int>(db.get_db(), "SELECT WayID, NodeID, SequenceNo FROM WayNodes");
    auto edges2 = psql::query_sql<int64_t, int64_t, int64_t>(db.get_db(), "SELECT WayID, StartNodeID, EndNodeID FROM Edges");
    std::sort(waynodes2.begin(), waynodes2.end());
    auto ndattrs2 = psql::query_sql<int64_t, std::string, std::string>(db.get_db(), "SELECT NodeID, Key, Value FROM NodeAttributes");
    auto wattrs2 = psql::query_sql<int64_t, std::string, std::string>(db.get_db(), "SELECT WayID, Key, Value FROM WayAttributes");
    auto rels2 = psql::query_sql<int64_t> (db.get_db(), "SELECT ID FROM Relations");
    auto rattrs2 = psql::query_sql<int64_t, std::string, std::string>(db.get_db(), "SELECT RelationID, Key, Value FROM RelationAttributes");
    auto nmembers2 = psql::query_sql<int64_t, std::string, int64_t>(db.get_db(), "SELECT RelationID, Role, NodeID FROM MemberNodes");
    auto wmembers2 = psql::query_sql<int64_t, std::string, int64_t>(db.get_db(), "SELECT RelationID, Role, WayID FROM MemberWays");
    auto rmembers2 = psql::query_sql<int64_t, std::string, int64_t>(db.get_db(), "SELECT ParentID, Role, ChildID FROM MemberRelations");
    BOOST_CHECK(nodes == nodes2);
    BOOST_CHECK(ways == ways2);
    BOOST_CHECK(edges == edges2);
    BOOST_CHECK(waynodes == waynodes2);
    BOOST_CHECK(ndattrs == ndattrs2);
    BOOST_CHECK(wattrs == wattrs2);
    BOOST_CHECK(rmembers == rmembers2);
    BOOST_CHECK(wmembers == wmembers2);
    BOOST_CHECK(nmembers == nmembers2);
    BOOST_CHECK(rattrs == rattrs2);
    BOOST_CHECK(rels == rels2);

}

BOOST_AUTO_TEST_CASE(new_import)
{

    osmdb::OsmDatabase db(pdb);
    db.create_tables();
    db.create_indexes_and_keys();
    osmdb::ElementCopy ins(db);
    ins.start_copy();
    osm::Node n(123, 1, 2);
    n.tags.insert(osm::Tag("nkey", "nval"));
    ins.insert_node(n);
    n.tags.clear();
    n.id = 124;
    ins.insert_node(n);
    osm::Way w(341);
    w.nodes.push_back(123);
    w.nodes.push_back(124);
    w.tags.insert(osm::Tag("\r\n\twkey", "wval"));
    ins.insert_way(w);
    osm::Relation r(432);
    r.tags.insert(osm::Tag("rkey", "rval"));
    ins.insert_relation(r);
    r.tags.clear();
    r.id = 433;
    r.add_node("rnode", osm::Node(124));
    r.add_way("rway", osm::Way(341));
    r.add_rel("rrel", osm::Relation(432));
    ins.insert_relation(r);
    ins.end_copy();
    osmdb::ImportTableProcessor proc(db);
    proc.process();
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
    std::vector<std::tuple<int64_t, std::string, std::string> > wattrs {std::make_tuple(341, "\r\n\twkey", "wval")};
    std::vector<std::tuple<int64_t> > rels {std::make_tuple(432), std::make_tuple(433)};
    std::vector<std::tuple<int64_t, std::string, std::string> > rattrs {std::make_tuple(432, "rkey", "rval")};
    std::vector<std::tuple<int64_t, std::string, int64_t> > nmembers {std::make_tuple(433, "rnode", 124)};
    std::vector<std::tuple<int64_t, std::string, int64_t> > wmembers {std::make_tuple(433, "rway", 341)};
    std::vector<std::tuple<int64_t, std::string, int64_t> > rmembers {std::make_tuple(433, "rrel", 432)};
    auto nodes2 = psql::query_sql<int64_t, double, double>(db.get_db(), "SELECT ID, ST_X(Location::geometry), ST_Y(Location::geometry) FROM Nodes");
    auto ways2 = psql::query_sql<int64_t>(db.get_db(), "SELECT ID FROM Ways");
    auto waynodes2 = psql::query_sql<int64_t, int64_t, int>(db.get_db(), "SELECT WayID, NodeID, SequenceNo FROM WayNodes");
    auto edges2 = psql::query_sql<int64_t, int64_t, int64_t>(db.get_db(), "SELECT WayID, StartNodeID, EndNodeID FROM Edges");
    std::sort(waynodes2.begin(), waynodes2.end());
    auto ndattrs2 = psql::query_sql<int64_t, std::string, std::string>(db.get_db(), "SELECT NodeID, Key, Value FROM NodeAttributes");
    auto wattrs2 = psql::query_sql<int64_t, std::string, std::string>(db.get_db(), "SELECT WayID, Key, Value FROM WayAttributes");
    auto rels2 = psql::query_sql<int64_t> (db.get_db(), "SELECT ID FROM Relations");
    auto rattrs2 = psql::query_sql<int64_t, std::string, std::string>(db.get_db(), "SELECT RelationID, Key, Value FROM RelationAttributes");
    auto nmembers2 = psql::query_sql<int64_t, std::string, int64_t>(db.get_db(), "SELECT RelationID, Role, NodeID FROM MemberNodes");
    auto wmembers2 = psql::query_sql<int64_t, std::string, int64_t>(db.get_db(), "SELECT RelationID, Role, WayID FROM MemberWays");
    auto rmembers2 = psql::query_sql<int64_t, std::string, int64_t>(db.get_db(), "SELECT ParentID, Role, ChildID FROM MemberRelations");
    BOOST_CHECK(nodes == nodes2);
    BOOST_CHECK(ways == ways2);
    BOOST_CHECK(edges == edges2);
    BOOST_CHECK(waynodes == waynodes2);
    BOOST_CHECK(ndattrs == ndattrs2);
    BOOST_CHECK(wattrs == wattrs2);
    BOOST_CHECK(rmembers == rmembers2);
    BOOST_CHECK(wmembers == wmembers2);
    BOOST_CHECK(nmembers == nmembers2);
    BOOST_CHECK(rattrs == rattrs2);
    BOOST_CHECK(rels == rels2);
}

BOOST_AUTO_TEST_CASE(empty_displaydb)
{
    osmdb::OsmDatabase odb(pdb);
    odb.create_tables();
    odb.create_indexes_and_keys();
    osmdb::DisplayDB db(odb, TEST_TO_SHOW_EDGES, 1, 1);
    db.set_bounds(geo::Point(0, 0), geo::Point(1, 1), 1);
}

BOOST_AUTO_TEST_CASE(simple)
{
    osmdb::OsmDatabase odb(pdb);
    odb.create_tables();
    odb.create_indexes_and_keys();
    osmdb::DisplayDB db(odb, TEST_TO_SHOW_EDGES, 1, 1);
    osmdb::ElementInsertion ins(db.get_db());
    pdb.begin_transaction();
    osm::Node nd(1, 0.5, 0.5);
    nd.tags.insert(osm::Tag("key", "val"));
    ins.insert_node(nd);
    ins.insert_node(osm::Node(2, 0.4, 0.4));
    ins.insert_node(osm::Node(3, 0.4, 0.8));
    osm::Way w(1);
    w.nodes.push_back(2);
    w.nodes.push_back(3);
    w.tags.insert(osm::Tag("key", "val"));
    ins.insert_way(w);
    nd.id = 4;
    nd.position.lat = 1.5;
    nd.position.lon = 0.5;
    ins.insert_node(nd);
    ins.insert_node(osm::Node(5, 0.6, 0.4));
    pdb.commit_transaction();
    db.set_bounds(geo::Point(1, 0), geo::Point(0, 1), 1);
    BOOST_CHECK(db.get_display_elements().size() == 1);
    auto elems = db.get_selected(geo::Point(1, 0), geo::Point(0, 1), 1);
    BOOST_CHECK(elems.size() == 1);
    BOOST_CHECK(elems[0]->get_description().front().first == "way");
}

BOOST_AUTO_TEST_CASE(wayred_cursor)
{
	osmdb::OsmDatabase odb(pdb);
	odb.create_tables();
	odb.create_indexes_and_keys();
	osmdb::ElementInsertion ins(odb);
	pdb.begin_transaction();
	osm::Node nd(1, 0.5, 0.5);
	nd.tags.insert(osm::Tag("key", "val"));
	ins.insert_node(nd);
	ins.insert_node(osm::Node(2, 0.4, 0.4));
	ins.insert_node(osm::Node(3, 0.4, 0.8));
	osm::Way w(1);
	w.nodes.push_back(2);
	w.nodes.push_back(3);
	w.tags.insert(osm::Tag("key", "val"));
	ins.insert_way(w);
	w=osm::Way(2);
	nd.id = 4;
	nd.position.lat = 1.5;
	nd.position.lon = 0.5;
	ins.insert_node(nd);
	ins.insert_node(osm::Node(5, 0.6, 0.4));
	pdb.commit_transaction();
	boost::property_tree::ptree entries;
	boost::property_tree::xml_parser::read_xml(TEST_REDUCT_PATH, entries, boost::property_tree::xml_parser::trim_whitespace);
	auto st(sqllib::get_decl_wayred_crs(entries, pdb));
	auto crs(psql::make_cursor(pdb, "wayred_crs", st));
	pdb.begin_transaction();
	crs.open();
	crs.fetch(10);
	auto vect(crs.get_buffer());
	BOOST_CHECK(vect.size()==2);
	crs.close();
	pdb.rollback_transaction();
}

BOOST_AUTO_TEST_CASE(properties)
{
    osmdb::OsmDatabase odb(pdb);
    odb.create_tables();
    osmdb::ElementInsertion ins(odb);
    osm::Node n(123, 1, 2);
    n.tags.insert(osm::Tag("nkey", "nval"));
    ins.insert_node(n);
    n.tags.clear();
    n.id = 124;
    ins.insert_node(n);
    osm::Way w(341);
    w.nodes.push_back(123);
    w.nodes.push_back(124);
    w.tags.insert(osm::Tag("wkey", "wval"));
    ins.insert_way(w);
    osm::Relation r(432);
    r.tags.insert(osm::Tag("rkey", "rval"));
    r.add_node("rnode", osm::Node(124));
    r.add_way("rway", osm::Way(341));
    r.add_rel("rrel", osm::Relation(433));
    ins.insert_relation(r);
    osmdb::PropertiesSelection db(odb);
    BOOST_CHECK(db.get_node_tags(123).count("nkey") == 1);
    geo::Point p(1, 2);
    BOOST_CHECK(db.get_position(123).close(p, 0.0001));
    BOOST_CHECK(db.get_way_tags(341).count("wkey") == 1);
    BOOST_CHECK(db.get_waynodes(341).size() == 2);
    BOOST_CHECK(db.get_waynodes(341)[0] == 123);
    BOOST_CHECK(db.get_relation_tags(432).count("rkey") == 1);
    BOOST_CHECK(db.get_node_members(432).count("rnode") == 1);
    BOOST_CHECK(db.get_way_members(432).count("rway") == 1);
    BOOST_CHECK(db.get_relation_members(432).count("rrel") == 1);
    osm::Node n2(123);
    n2.fill(db);
    n.id = 123;
    n.tags.insert(osm::Tag("nkey", "nval"));
    BOOST_CHECK(n == n2);
    osm::Way w2(341);
    w2.fill(db);
    w.nodes[0].fill(db);
    w.nodes[1].fill(db);
    BOOST_CHECK(w == w2);
    osm::Relation r2(432);
    r2.fill(db);
    for (auto it = r.members.begin(); it != r.members.end(); ++it)
        it->second->fill(db);
    BOOST_CHECK(r == r2);
}

BOOST_AUTO_TEST_CASE(waylister)
{
	osmdb::OsmDatabase odb(pdb);
	odb.create_tables();
	odb.create_indexes_and_keys();
	osmdb::ElementInsertion ins(odb);
	pdb.begin_transaction();
	ins.insert_node(osm::Node(2, 0.4, 0.4));
	ins.insert_node(osm::Node(3, 0.4, 0.8));
	ins.insert_node(osm::Node(4, 0.4, 0.8));
	ins.insert_node(osm::Node(5, 0.4, 0.8));
	osm::Way w(1);
	w.nodes.push_back(osm::Node(2, 0.4, 0.4));
	w.nodes.push_back(osm::Node(3, 0.4, 0.8));
	w.nodes.push_back(osm::Node(4, 0.4, 0.8));
	w.tags.insert(osm::Tag("key", "val"));
	ins.insert_way(w);
	w.tags.clear();
	std::vector<osm::Way> exp{w};
	w.id = 2;
	w.nodes.clear();
	w.nodes.push_back(3);
	w.nodes.push_back(5);
	w.tags.clear();
	w.tags.insert(osm::Tag("asdf", "bsdf"));
	w.tags.insert(osm::Tag("fcda", "gas"));
	ins.insert_way(w);
	pdb.commit_transaction();
	pdb.begin_transaction();
	osmdb::WayLister wl(odb, TEST_REDUCT_PATH, 2);
	w.nodes.clear();
	std::multimap<osm::Node, osm::Way, osm::LtByID> exp_mp;
	exp_mp.insert(std::make_pair(osm::Node(3, 0.4, 0.8), w));
	std::multimap<osm::Node, osm::Way, osm::LtByID> got_mp;
	std::vector<osm::Way> got;
	while(!wl.end())
	{
		wl.next();
		for(auto it = wl.get_current_connected_ways().begin(); it!=wl.get_current_connected_ways().end(); ++it)
		{
			got_mp.insert(*it);
		}
		for(unsigned int i = 0; i<wl.get_current_ways().size(); ++i)
		{
			got.push_back(wl.get_current_ways()[i]);
		}
	}
	pdb.commit_transaction();
	BOOST_CHECK(exp==got);
	BOOST_CHECK(exp_mp == got_mp);
}

BOOST_AUTO_TEST_SUITE_END()
