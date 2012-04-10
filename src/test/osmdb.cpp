/*
 * osmdb.cpp
 *
 *  Created on: Nov 6, 2011
 *      Author: martin
 */

#include <boost/test/unit_test.hpp>
#include <boost/filesystem.hpp>
#include <boost/property_tree/xml_parser.hpp>
#include <boost/range/algorithm.hpp>
#include "../osmdb/osmdb.h"
#include "../sqllib/sqllib.h"
#include <test_config.h>
#include <algorithm>
#include "../util/range.h"

class OsmDBFixture
{
public:
    OsmDBFixture():
        pdb(""),
        pdb2("")
    {
        psql::execute_sql(pdb, "CREATE SCHEMA testing");
        psql::execute_sql(pdb, "SET search_path TO testing, public");
        psql::execute_sql(pdb2, "CREATE SCHEMA testing2");
        psql::execute_sql(pdb2, "SET search_path TO testing2, testing, public");
    }
    ~OsmDBFixture()
    {
        if (pdb.in_transaction() || pdb.in_failed_transaction())
            pdb.rollback_transaction();
        psql::execute_sql(pdb, "DROP SCHEMA testing CASCADE");
        if (pdb2.in_transaction() || pdb2.in_failed_transaction())
            pdb2.rollback_transaction();
        psql::execute_sql(pdb2, "DROP SCHEMA testing2 CASCADE");

    }
    psql::Database pdb;
    psql::Database pdb2;
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
    w.add_node(123);
    w.add_node(124);
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
    std::vector<std::tuple<int64_t, int64_t, int, int> > waynodes
    {
        std::make_tuple(341, 123, 0, 1),
        std::make_tuple(341, 124, 1, -1)
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
    auto waynodes2 = psql::query_sql<int64_t, int64_t, int, int>(db.get_db(), "SELECT WayID, NodeID, SequenceNo, COALESCE(NextSequenceNo, -1) FROM WayNodes");
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
    w.add_node(123);
    w.add_node(124);
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
    std::vector<std::tuple<int64_t, int64_t, int, int> > waynodes
    {
        std::make_tuple(341, 123, 0, 1),
        std::make_tuple(341, 124, 1, -1)
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
    auto waynodes2 = psql::query_sql<int64_t, int64_t, int, int>(db.get_db(), "SELECT WayID, NodeID, SequenceNo, COALESCE(NextSequenceNo, -1) FROM WayNodes");
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
    BOOST_CHECK(waynodes == waynodes2);
    BOOST_CHECK(ndattrs == ndattrs2);
    BOOST_CHECK(wattrs == wattrs2);
    BOOST_CHECK(rmembers == rmembers2);
    BOOST_CHECK(wmembers == wmembers2);
    BOOST_CHECK(nmembers == nmembers2);
    BOOST_CHECK(rattrs == rattrs2);
    BOOST_CHECK(rels == rels2);
}

BOOST_AUTO_TEST_CASE(edge_creator)
{
    osmdb::OsmDatabase db(pdb);
    db.create_tables();
    db.create_indexes_and_keys();
    osmdb::ElementInsertion ins(db);
    osm::Way w(2);
    w.add_node(1);
    w.add_node(2);
    w.tags.insert(osm::Tag("key", "val"));
    ins.insert_node(1);
    ins.insert_node(2);
    ins.insert_node(3);
    ins.insert_way(w);
    w = osm::Way(1);
    w.add_node(2);
    w.add_node(3);
    w.tags.insert(osm::Tag("key", "val"));
    ins.insert_way(w);
    boost::property_tree::ptree tr;
    boost::property_tree::xml_parser::read_xml(TEST_TO_SHOW_EDGES, tr, boost::property_tree::xml_parser::trim_whitespace);
    osmdb::EdgeCreator ecr(db);
    ecr.create_tables();
    ecr.create_keys_and_indexes();
    ecr.insert_data(tr);
    auto v = psql::query_sql<int64_t, int, int, int64_t, int64_t>(db.get_db(), "SELECT WayID, StartSequenceNo, EndSequenceNo, StartNodeID, EndNodeID FROM Edges");
    BOOST_CHECK(v.size() == 2);
}

BOOST_AUTO_TEST_CASE(import_proc_dis)
{
    osmdb::OsmDatabase db(pdb);
    osmdb::ImportTableProcessor proc(db);
    proc.disable_all();
    proc.action_signal.connect(
        [&](osmdb::ImportTableAction, int64_t)
    {
        BOOST_CHECK(false);
    });

}

BOOST_AUTO_TEST_CASE(empty_displaydb)
{
    osmdb::OsmDatabase odb(pdb);
    boost::property_tree::ptree tr;
    boost::property_tree::xml_parser::read_xml(TEST_TO_SHOW_EDGES, tr, boost::property_tree::xml_parser::trim_whitespace);
    osmdb::EdgeCreator ecr(odb);
    odb.create_tables();
    odb.create_indexes_and_keys();
    ecr.create_tables();
    ecr.create_keys_and_indexes();
    osmdb::DisplayDB db(odb, std::vector<std::string> {"testing"}, 1, std::shared_ptr<osmdb::EdgeTranslator>(new osmdb::ElementEdgeTranslator(odb)));
    db.set_bounds(geo::Point(0, 0), geo::Point(1, 1), 1);
}

BOOST_AUTO_TEST_CASE(simple_dispdb)
{
    osmdb::OsmDatabase odb(pdb);
    odb.create_tables();
    odb.create_indexes_and_keys();
    boost::property_tree::ptree tr;
    boost::property_tree::xml_parser::read_xml(TEST_TO_SHOW_EDGES, tr, boost::property_tree::xml_parser::trim_whitespace);
    osmdb::EdgeCreator ecr(odb);
    ecr.create_tables();
    osmdb::DisplayDB db(odb, std::vector<std::string> {"testing"}, 1, std::shared_ptr<osmdb::EdgeTranslator>(new osmdb::ElementEdgeTranslator(odb)));
    osmdb::ElementInsertion ins(db.get_db());
    pdb.begin_transaction();
    osm::Node nd(1, 0.5, 0.5);
    nd.tags.insert(osm::Tag("key", "val"));
    ins.insert_node(nd);
    ins.insert_node(osm::Node(2, 0.4, 0.4));
    ins.insert_node(osm::Node(3, 0.4, 0.8));
    osm::Way w(1);
    w.add_node(2);
    w.add_node(3);
    w.tags.insert(osm::Tag("key", "val"));
    ins.insert_way(w);
    nd.id = 4;
    nd.position.lat = 1.5;
    nd.position.lon = 0.5;
    ins.insert_node(nd);
    ins.insert_node(osm::Node(5, 0.6, 0.4));
    w = osm::Way(2);
    w.add_node(osm::Node(3));
    w.add_node(osm::Node(5));
    ins.insert_way(w);
    ecr.insert_data(tr);
    pdb.commit_transaction();
    db.set_bounds(geo::Point(1, 0), geo::Point(0, 1), 1);
    BOOST_CHECK(util::count(db.get_display_elements()) == 1);
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
    w.add_node(2);
    w.add_node(3);
    w.tags.insert(osm::Tag("key", "val"));
    ins.insert_way(w);
    w = osm::Way(2);
    nd.id = 4;
    nd.position.lat = 1.5;
    nd.position.lon = 0.5;
    ins.insert_node(nd);
    ins.insert_node(osm::Node(5, 0.6, 0.4));
    pdb.commit_transaction();
    boost::property_tree::ptree entries;
    boost::property_tree::ptree kv;
    kv.put("key", "key");
    kv.put("value", "val");
    entries.put_child("entries.entry.elements.el", kv);
    auto st(sqllib::get_wayreduction_select(entries, pdb));
    auto crs(psql::make_cursor(pdb, "wayred_crs", st));
    pdb.begin_transaction();
    crs.open();
    crs.fetch(10);
    auto vect(crs.get_buffer());
    BOOST_CHECK(vect.size() == 2);
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
    w.add_node(123);
    w.add_node(124);
    w.tags.insert(osm::Tag("wkey", "wval"));
    ins.insert_way(w);
    osm::Relation r(432);
    r.tags.insert(osm::Tag("rkey", "rval"));
    r.add_node("rnode", osm::Node(124));
    r.add_way("rway", osm::Way(341));
    r.add_rel("rrel", osm::Relation(433));
    ins.insert_relation(r);
    osmdb::PropertiesSelection db(odb);
    BOOST_CHECK(db.get_node_tags(123).count(osm::Tag("nkey", "nval")) == 1);
    geo::Point p(1, 2);
    BOOST_CHECK(db.get_position(123).close(p, 0.0001));
    BOOST_CHECK(db.get_way_tags(341).count(osm::Tag("wkey", "wval")) == 1);
    BOOST_CHECK(db.get_waynodes(341).size() == 2);
    BOOST_CHECK(db.get_waynodes(341)[0] == 123);
    BOOST_CHECK(db.get_relation_tags(432).count(osm::Tag("rkey", "rval")) == 1);
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

void write_ptree(boost::property_tree::ptree const& ptree, std::ostream& ost, int depth)
{
    for (auto it = ptree.begin(); it != ptree.end(); ++it)
    {
        if (it->second.data() == "" && it->second.size() == 0)
            continue;
        for (int i = 0; i < depth; ++i)
            ost << "\t";
        ost << it->first << " ";
        ost << it->second.data();
        ost << std::endl;
        write_ptree(it->second, ost, depth + 1);
    }
}

void write_ptree_a(boost::property_tree::ptree const& ptree, std::ostream& ost, int depth)
{
    for (auto it = ptree.begin(); it != ptree.end(); ++it)
    {
        if (it->second.data() == "" && it->second.size() == 0)
            continue;
        for (int i = 0; i < depth; ++i)
            ost << "\t";
        ost << it->first << " ";
        ost << it->second.data();
        ost << std::endl;
        write_ptree(it->second, ost, depth + 1);
    }
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
    ins.insert_node(osm::Node(6, 0.4, 0.8));
    ins.insert_node(osm::Node(7, 0.4, 0.8));
    ins.insert_node(osm::Node(8, 0.4, 0.8));
    osm::Way w(1);
    w.add_node(osm::Node(2, 0.4, 0.4));
    w.add_node(osm::Node(3, 0.4, 0.8));
    w.add_node(osm::Node(4, 0.4, 0.8));
    w.tags.insert(osm::Tag("key", "val"));
    ins.insert_way(w);
    osm::Way w2(2);
    w2.add_node(3);
    w2.add_node(5);
    w2.tags.insert(osm::Tag("asdf", "bsdf"));
    w2.tags.insert(osm::Tag("fcda", "gas"));
    ins.insert_way(w2);
    osm::Way w3(3);
    w3.tags.insert(osm::Tag("key", "val"));
    w3.add_node(osm::Node(4, 0.4, 0.8));
    w3.add_node(osm::Node(5, 0.4, 0.8));
    w3.add_node(osm::Node(6, 0.4, 0.8));
    ins.insert_way(w3);
    osm::Way w4(4);
    w4.add_node(5);
    w4.add_node(7);
    ins.insert_way(w4);
    osm::Way w5(5);
    w5.add_node(6);
    w5.add_node(8);
    ins.insert_way(w5);
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
    pdb.commit_transaction();
    pdb.begin_transaction();
    std::map<osm::Way, std::multimap<osm::Node, osm::Way, osm::LtByID>, osm::LtByID> got;
    osmdb::WayLister wl(odb, std::multimap<std::string, std::string> {std::make_pair("key", "val")});
    for (auto it = wl.get_range().begin(); it != wl.get_range().end(); ++it)
    {
        got.insert(*it);
    }
    pdb.commit_transaction();
    class WayCmpIgnNd
    {
    public:
        bool operator()(osm::Way const& w1, osm::Way const& w2)
        {
            return w1.id == w2.id && w1.tags == w2.tags;
        }
    };
    class Comp
    {
    public:
        bool operator()(decltype(*got.begin()) p1, decltype(*got.begin()) p2)
        {
            return osm::EqByID()(p1.first, p2.first) && util::multimap_eq<WayCmpIgnNd>(p1.second, p2.second);
        }
    } cmp;
    bool b = std::equal(got.begin(), got.end(), exp.begin(), cmp);
    BOOST_CHECK(b);
}

BOOST_AUTO_TEST_CASE(allwayslister)
{
    osm::Node n1(3, 2, 3);
    n1.tags.insert(osm::Tag("k1", "v1"));
    n1.tags.insert(osm::Tag("k2", "v2"));
    osm::Node n2(2, -4, 2);
    n2.tags.insert(osm::Tag("k3", "v3"));
    n2.tags.insert(osm::Tag("k4", "v4"));
    osm::Node n3(1, 5, -3);
    osm::Way w1(1);
    w1.tags.insert(osm::Tag("k5", "v5"));
    w1.tags.insert(osm::Tag("key", "val"));
    w1.add_node(n1);
    w1.add_node(n2);
    osm::Way w2(2);
    w2.tags.insert(osm::Tag("k6", "v6"));
    w2.tags.insert(osm::Tag("key", "val"));
    w2.add_node(n2);
    w2.add_node(n3);
    osm::Way w3(3);
    w3.add_node(n3);
    w3.add_node(n1);
    osmdb::OsmDatabase odb(pdb);
    odb.create_tables();
    osmdb::ElementInsertion ins(odb);
    ins.insert_node(n1);
    ins.insert_node(n2);
    ins.insert_node(n3);
    ins.insert_way(w1);
    ins.insert_way(w2);
    //ins.insert_way(w3);
    pdb.begin_transaction();
    osmdb::AllWayLister wl(odb, std::multimap<std::string, std::string> {std::make_pair("key", "val")});
    std::vector<osm::Way> exp {w1, w2};
    std::vector<osm::Way> got;
    for (auto it = wl.get_range().begin(); it != wl.get_range().end(); ++it)
    {
        got.push_back(*it);
    }
    BOOST_CHECK(got == exp);
    pdb.rollback_transaction();
}

BOOST_AUTO_TEST_CASE(road_create)
{
    psql::Database dest("");
    dest.set_schema("testing");
    osmdb::OsmDatabase red_odb(pdb2);
    osmdb::OsmDatabase full_odb(pdb);
    full_odb.create_tables();
    full_odb.create_indexes_and_keys();
    red_odb.create_tables();
    red_odb.create_indexes_and_keys();
    osmdb::OsmDatabase dest_odb(dest);
    osmdb::ElementInsertion full_ins(full_odb);
    osm::Node n1(1, 0, 0);
    osm::Node n2(2, 1, 0);
    osm::Node n3(3, 1, 1);
    osm::Node n4(4, 0, 1);
    std::vector<geo::Point> positions {n1.position, n2.position, n3.position, n4.position};
    full_ins.insert_node(n1);
    full_ins.insert_node(n2);
    full_ins.insert_node(n3);
    full_ins.insert_node(n4);
    osm::Way w1(1);
    w1.add_node(osm::Node(1));
    w1.add_node(osm::Node(2));
    w1.add_node(osm::Node(3));
    w1.add_node(osm::Node(4));
    w1.tags.insert(osm::Tag("k", "v"));
    full_ins.insert_way(w1);
    osmdb::ElementInsertion red_ins(red_odb);
    w1.nodes.erase(1);
    red_ins.insert_node(n1);
    red_ins.insert_node(n2);
    red_ins.insert_node(n3);
    red_ins.insert_node(n4);
    red_ins.insert_way(w1);
    osmdb::RoadNetworkCreator rnc(full_odb, red_odb, dest_odb, std::multimap<std::string, std::string> {std::make_pair("k", "v")});
    rnc.create_road_network_table();
    rnc.copy_road_network_data();
    auto st = sqllib::get_select_road_edges(dest);
    st.execute();
    BOOST_CHECK(st.row_count() == 4);
    bool b = false;
    for (int i = 0; i < st.row_count(); ++i)
    {
        auto tup = st.get_row(i);
        geo::Point p1(std::get<2>(tup), std::get<1>(tup));
        geo::Point p2(std::get<5>(tup), std::get<4>(tup));
        BOOST_CHECK(boost::range::find(positions, p1) != positions.end());
        BOOST_CHECK(boost::range::find(positions, p2) != positions.end());
        int i1 = std::get<7>(tup);
        int i2 = std::get<8>(tup);
        if (i2 - i1 == 2)
            b = true;
    }
    BOOST_CHECK(b);
}

BOOST_AUTO_TEST_SUITE_END()

