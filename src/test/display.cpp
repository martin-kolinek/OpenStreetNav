/*
 * display.cpp
 *
 *  Created on: Mar 1, 2012
 *      Author: martin
 */

#include <boost/test/unit_test.hpp>
#include <test_config.h>
#include "../osmdb/osmdb.h"
#include "../displayer/EdgeHighlighter.h"
#include "../util/range.h"

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
        if (pdb.in_transaction() || pdb.in_failed_transaction())
            pdb.rollback_transaction();
        psql::execute_sql(pdb, "DROP SCHEMA testing CASCADE");
    }
    psql::Database pdb;
};

BOOST_FIXTURE_TEST_SUITE(display_test, OsmDBFixture)

BOOST_AUTO_TEST_CASE(highlight)
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
    display::EdgeHighlighter high(db, display::LineDisplayStyle(1, 1, 1, 1, 0, false));
    high.add_descriptible(w);
    w = osm::Way(2);
    w.nodes.push_back(osm::Node(3));
    w.nodes.push_back(osm::Node(5));
    w.tags.insert(osm::Tag("key", "val"));
    ins.insert_way(w);
    pdb.commit_transaction();
    db.set_bounds(geo::Point(1, 0), geo::Point(0, 1), 1);
    high.set_bounds(geo::Point(1, 0), geo::Point(0, 1), 1);
    BOOST_CHECK(util::count(high.get_display_elements()) == 1);
}

BOOST_AUTO_TEST_SUITE_END()


