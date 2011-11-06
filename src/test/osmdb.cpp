/*
 * osmdb.cpp
 *
 *  Created on: Nov 6, 2011
 *      Author: martin
 */

#include <boost/test/unit_test.hpp>
#include <boost/filesystem.hpp>
#include "../osmdb/OsmDatabase.h"
#include "../osmdb/osmdb.h"

class OsmDBFixture
{
public:
    OsmDBFixture()
    {
        BOOST_REQUIRE_MESSAGE(!boost::filesystem::exists("test.db"), "test.db exists, aborting");
    }
    ~OsmDBFixture()
    {
        remove("test.db");
    }
};

BOOST_FIXTURE_TEST_SUITE(OsmDBCreateTests, OsmDBFixture)

BOOST_AUTO_TEST_CASE(create)
{
    {
        osmdb::OsmDatabase db("test.db");
    }
    {
        osmdb::OsmDatabase db("test.db");
    }
}

BOOST_AUTO_TEST_CASE(indexes)
{
    osmdb::OsmDatabase db("test.db");
    db.create_indexes();
}

void create_db()
{
    osmdb::OsmDatabase db("test.db");
}

BOOST_AUTO_TEST_CASE(error)
{
    {
        sqlite::Database db("test.db");
        sqlite::execute_sql("CREATE TABLE asdf (ID INTEGER)", db);
    }
    BOOST_CHECK_THROW(create_db(), osmdb::WrongDBException);
}

BOOST_AUTO_TEST_CASE(insert)
{
    osmdb::OsmDatabase db("test.db");
    osmdb::ElementInsertion ins(db);
    osm::Node n(123, 1, 2);
    n.tags.push_back(osm::Tag("nkey", "nval"));
    ins.insert_node(n);
    osm::Way w(341);
    w.nodes.push_back(444);
    w.nodes.push_back(445);
    w.tags.push_back(osm::Tag("wkey", "wval"));
    ins.insert_way(w);
    osm::Relation r(513);
    r.members.push_back(osm::RelationMapping("role", osm::ObjectType::Node, 142));
    r.tags.push_back(osm::Tag("rkey", "rval"));
    ins.insert_relation(r);
    std::vector<std::tuple<int64_t, double, double> > nodes {std::make_tuple(123, 1, 2)};
    std::vector<std::tuple<int64_t> > ways {std::make_tuple(341)};
    std::vector<std::tuple<int64_t, int64_t, int64_t> > edges {std::make_tuple(341, 444, 445)};
    std::vector<std::tuple<int64_t> > rels {std::make_tuple(513)};
    std::vector<std::tuple<int64_t, std::string, int, int64_t> > rel_cont {std::make_tuple(513, "role", (int)osm::ObjectType::Node, 142)};
    std::vector<std::tuple<int, int64_t, std::string, std::string> > attrs
    {
        std::make_tuple((int)osm::ObjectType::Node, 123, "nkey", "nval"),
        std::make_tuple((int)osm::ObjectType::Way, 341, "wkey", "wval"),
        std::make_tuple((int)osm::ObjectType::Relation, 513, "rkey", "rval")
    };
    std::sort(attrs.begin(), attrs.end());
    auto nodes2 = sqlite::query_sql("SELECT ID, Latitude, Longitude FROM " + db.nodes_table, db.get_db(), sqlite::colint64(), sqlite::coldouble(), sqlite::coldouble());
    auto ways2 = sqlite::query_sql("SELECT ID FROM " + db.ways_table, db.get_db(), sqlite::colint64());
    auto edges2 = sqlite::query_sql("SELECT WayID, StartNodeID, EndNodeID FROM " + db.edges_table, db.get_db(), sqlite::colint64(), sqlite::colint64(), sqlite::colint64());
    auto rels2 = sqlite::query_sql("SELECT ID FROM " + db.relations_table, db.get_db(), sqlite::colint64());
    auto rel_cont2 = sqlite::query_sql("SELECT RelationID, Role, ObjectType, ObjectID FROM " + db.relation_contents_table, db.get_db(), sqlite::colint64(), sqlite::colstr(), sqlite::colint(), sqlite::colint64());
    auto attrs2 = sqlite::query_sql("SELECT ObjectType, ObjectID, Key, Value FROM " + db.attributes_table, db.get_db(), sqlite::colint(), sqlite::colint64(), sqlite::colstr(), sqlite::colstr());
    std::sort(attrs2.begin(), attrs2.end());
    BOOST_CHECK(nodes == nodes2);
    BOOST_CHECK(ways == ways2);
    BOOST_CHECK(edges == edges2);
    BOOST_CHECK(rels == rels2);
    BOOST_CHECK(rel_cont == rel_cont2);
    BOOST_CHECK(attrs == attrs2);
}

BOOST_AUTO_TEST_SUITE_END()
