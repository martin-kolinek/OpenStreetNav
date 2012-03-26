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
#include <boost/property_tree/xml_parser.hpp>

class disp_db_fix
{
public:
    disp_db_fix():
        pdb("")
    {

        psql::execute_sql(pdb, "CREATE SCHEMA testing");
        psql::execute_sql(pdb, "SET search_path TO testing, public");
    }
    ~disp_db_fix()
    {
        if (pdb.in_transaction() || pdb.in_failed_transaction())
            pdb.rollback_transaction();
        psql::execute_sql(pdb, "DROP SCHEMA testing CASCADE");
    }
    psql::Database pdb;
};

BOOST_FIXTURE_TEST_SUITE(display_test, disp_db_fix)

BOOST_AUTO_TEST_CASE(highlight)
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
    nd.id = 4;
    nd.position.lat = 1.5;
    nd.position.lon = 0.5;
    ins.insert_node(nd);
    ins.insert_node(osm::Node(5, 0.6, 0.4));
    osm::Way w2(2);
    w2.add_node(osm::Node(3));
    w2.add_node(osm::Node(5));
    w2.tags.insert(osm::Tag("key", "val"));
    ins.insert_way(w2);
    boost::property_tree::ptree tr;
    boost::property_tree::xml_parser::read_xml(TEST_TO_SHOW_EDGES, tr, boost::property_tree::xml_parser::trim_whitespace);
    osmdb::EdgeCreator ecr(odb, tr);
    ecr.create_tables();
    ecr.insert_data();
    pdb.commit_transaction();
    osmdb::DisplayDB db(odb, std::vector<std::string> {"testing"}, 1);
    display::EdgeHighlighter high(db, display::LineDisplayStyle(1, 1, 1, 1, 0, false));
    high.add_descriptible(w);
    db.set_bounds(geo::Point(1, 0), geo::Point(0, 1), 1);
    high.set_bounds(geo::Point(1, 0), geo::Point(0, 1), 1);
    BOOST_CHECK(util::count(high.get_display_elements()) == 1);
}

BOOST_AUTO_TEST_SUITE_END()


