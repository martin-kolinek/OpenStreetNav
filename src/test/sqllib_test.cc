/*
 * test_sqllib.cc
 *
 *  Created on: Nov 23, 2011
 *      Author: martin
 */

#include <boost/test/unit_test.hpp>
#include "../sqllib/sqllib.h"
#include "../wkb/wkb.h"
#include <iostream>

class SqlLibFixture
{
public:
    psql::Database db;
    SqlLibFixture():
        db("")
    {
        psql::Statement<psql::BindTypes<>, psql::RetTypes<> > st("CREATE SCHEMA testing", db);
        st.execute();
        st = psql::Statement<psql::BindTypes<>, psql::RetTypes<> >("SET search_path TO testing, public", db);
        st.execute();
    }
    ~SqlLibFixture()
    {
        try
        {
            psql::Statement<psql::BindTypes<>, psql::RetTypes<> > st("DROP SCHEMA testing CASCADE", db);
            st.execute();
        }
        catch (psql::PgSqlException& ex)
        {
            std::cout << "WARNING: problem deleting testing schema " << ex.what() << std::endl;
        }
    }
};

BOOST_FIXTURE_TEST_SUITE(sqllib, SqlLibFixture)

BOOST_AUTO_TEST_CASE(lib_create_edges_location_index)
{
    auto st0(sqllib::get_create_nodes_table(db, false));
    st0.execute();
    auto st1(sqllib::get_create_ways_table(db, false));
    st1.execute();
    auto st2(sqllib::get_create_edges_table(db, false));
    st2.execute();
    auto tested_st(sqllib::get_create_edges_location_index(db));
    tested_st.execute();
}

BOOST_AUTO_TEST_CASE(lib_create_edges_table)
{
    auto st0(sqllib::get_create_ways_table(db, false));
    st0.execute();
    auto st1(sqllib::get_create_nodes_table(db, false));
    st1.execute();
    auto tested_st(sqllib::get_create_edges_table(db));
    tested_st.execute();
}

BOOST_AUTO_TEST_CASE(lib_create_node_attributes)
{
    auto st0(sqllib::get_create_nodes_table(db, false));
    st0.execute();
    auto tested_st(sqllib::get_create_node_attributes(db));
    tested_st.execute();
}

BOOST_AUTO_TEST_CASE(lib_create_node_members)
{
    auto st0(sqllib::get_create_relations_table(db, false));
    st0.execute();
    auto st1(sqllib::get_create_nodes_table(db, false));
    st1.execute();
    auto tested_st(sqllib::get_create_node_members(db));
    tested_st.execute();
}

BOOST_AUTO_TEST_CASE(lib_create_nodes_loc_index)
{
    auto st0(sqllib::get_create_nodes_table(db, false));
    st0.execute();
    auto tested_st(sqllib::get_create_nodes_loc_index(db));
    tested_st.execute();
}

BOOST_AUTO_TEST_CASE(lib_create_nodes_table)
{
    auto tested_st(sqllib::get_create_nodes_table(db));
    tested_st.execute();
}

BOOST_AUTO_TEST_CASE(lib_create_relation_attributes)
{
    auto st0(sqllib::get_create_relations_table(db, false));
    st0.execute();
    auto tested_st(sqllib::get_create_relation_attributes(db));
    tested_st.execute();
}

BOOST_AUTO_TEST_CASE(lib_create_relations_table)
{
    auto tested_st(sqllib::get_create_relations_table(db));
    tested_st.execute();
}

BOOST_AUTO_TEST_CASE(lib_create_test_table)
{
    auto tested_st(sqllib::get_create_test_table(db));
    tested_st.execute();
}

BOOST_AUTO_TEST_CASE(lib_create_toshow_table)
{
    auto tested_st(sqllib::get_create_toshow_table(db));
    tested_st.execute();
}

BOOST_AUTO_TEST_CASE(lib_create_way_attributes)
{
    auto st0(sqllib::get_create_ways_table(db, false));
    st0.execute();
    auto tested_st(sqllib::get_create_way_attributes(db));
    tested_st.execute();
}

BOOST_AUTO_TEST_CASE(lib_create_way_members_table)
{
    auto st0(sqllib::get_create_relations_table(db, false));
    st0.execute();
    auto st1(sqllib::get_create_ways_table(db, false));
    st1.execute();
    auto tested_st(sqllib::get_create_way_members_table(db));
    tested_st.execute();
}

BOOST_AUTO_TEST_CASE(lib_create_waynodes_table)
{
    auto st0(sqllib::get_create_ways_table(db, false));
    st0.execute();
    auto st1(sqllib::get_create_nodes_table(db, false));
    st1.execute();
    auto tested_st(sqllib::get_create_waynodes_table(db));
    tested_st.execute();
}

BOOST_AUTO_TEST_CASE(lib_create_ways_table)
{
    auto tested_st(sqllib::get_create_ways_table(db));
    tested_st.execute();
}

BOOST_AUTO_TEST_CASE(lib_insert_edge)
{
    auto st0(sqllib::get_create_ways_table(db, false));
    st0.execute();
    auto st1(sqllib::get_create_nodes_table(db, false));
    st1.execute();
    auto st2(sqllib::get_create_edges_table(db, false));
    st2.execute();
    auto st3(sqllib::get_insert_way(db, false));
    st3.execute(41);
    auto st4(sqllib::get_insert_node(db, false));
    st4.execute(10, 25, 40);
    auto st5(sqllib::get_insert_node(db, false));
    st5.execute(11, 20, 30);
    auto tested_st(sqllib::get_insert_edge(db));
    tested_st.execute(41, 10, 11);
}

BOOST_AUTO_TEST_CASE(lib_insert_node)
{
    auto st0(sqllib::get_create_nodes_table(db, false));
    st0.execute();
    auto tested_st(sqllib::get_insert_node(db));
    tested_st.execute(123, 10, 20);
}

BOOST_AUTO_TEST_CASE(lib_insert_node_attr)
{
    auto st0(sqllib::get_create_nodes_table(db, false));
    st0.execute();
    auto st1(sqllib::get_create_node_attributes(db, false));
    st1.execute();
    auto st2(sqllib::get_insert_node(db, false));
    st2.execute(10, 20, 30);
    auto tested_st(sqllib::get_insert_node_attr(db));
    tested_st.execute(10, "asdf", "bdas");
}

BOOST_AUTO_TEST_CASE(lib_insert_test_table)
{
    auto st0(sqllib::get_create_test_table(db, false));
    st0.execute();
    auto tested_st(sqllib::get_insert_test_table(db));
    tested_st.execute(10, "gda", 34422354323);
}

BOOST_AUTO_TEST_CASE(lib_insert_toshow)
{
    auto st0(sqllib::get_create_toshow_table(db, false));
    st0.execute();
    auto tested_st(sqllib::get_insert_toshow(db));
    tested_st.execute("asdf", "asdf", 5);
}

BOOST_AUTO_TEST_CASE(lib_insert_way)
{
    auto st0(sqllib::get_create_ways_table(db, false));
    st0.execute();
    auto tested_st(sqllib::get_insert_way(db));
    tested_st.execute(20);
}

BOOST_AUTO_TEST_CASE(lib_insert_way_attr)
{
    auto st0(sqllib::get_create_ways_table(db, false));
    st0.execute();
    auto st1(sqllib::get_create_way_attributes(db, false));
    st1.execute();
    auto st2(sqllib::get_insert_way(db, false));
    st2.execute(30);
    auto tested_st(sqllib::get_insert_way_attr(db));
    tested_st.execute(30, "afsa", "gdas");
}

BOOST_AUTO_TEST_CASE(lib_insert_way_node)
{
    auto st0(sqllib::get_create_ways_table(db, false));
    st0.execute();
    auto st1(sqllib::get_create_nodes_table(db, false));
    st1.execute();
    auto st2(sqllib::get_create_waynodes_table(db, false));
    st2.execute();
    auto st3(sqllib::get_insert_node(db, false));
    st3.execute(21, 30, 40);
    auto st4(sqllib::get_insert_way(db, false));
    st4.execute(20);
    auto tested_st(sqllib::get_insert_way_node(db));
    tested_st.execute(20, 21, 1);
}

BOOST_AUTO_TEST_CASE(lib_select_edges_in_box)
{
    auto st0(sqllib::get_create_ways_table(db, false));
    st0.execute();
    auto st1(sqllib::get_create_nodes_table(db, false));
    st1.execute();
    auto st2(sqllib::get_create_edges_table(db, false));
    st2.execute();
    auto st3(sqllib::get_insert_way(db, false));
    st3.execute(41);
    auto st4(sqllib::get_insert_node(db, false));
    st4.execute(10, 25, 40);
    auto st5(sqllib::get_insert_node(db, false));
    st5.execute(11, 20, 30);
    auto st6(sqllib::get_insert_edge(db, false));
    st6.execute(41, 10, 11);
    auto st7(sqllib::get_create_toshow_table(db, false));
    st7.execute();
    auto st8(sqllib::get_create_way_attributes(db, false));
    st8.execute();
    auto st9(sqllib::get_insert_way_attr(db, false));
    st9.execute(41, "asdf", "asdf");
    auto st10(sqllib::get_insert_toshow(db, false));
    st10.execute("asdf", "asdf", 5);
    auto tested_st(sqllib::get_select_edges_in_box(db));
    tested_st.execute(5, 0, 0, 50, 50);
    tested_st.get_row(0);
}

BOOST_AUTO_TEST_CASE(lib_select_node_descr_in_box)
{
    auto st0(sqllib::get_create_nodes_table(db, false));
    st0.execute();
    auto st1(sqllib::get_insert_node(db, false));
    st1.execute(10, 25, 40);
    auto st2(sqllib::get_create_toshow_table(db, false));
    st2.execute();
    auto st3(sqllib::get_create_node_attributes(db, false));
    st3.execute();
    auto st4(sqllib::get_insert_node_attr(db, false));
    st4.execute(10, "asdf", "asdf");
    auto st5(sqllib::get_insert_toshow(db, false));
    st5.execute("asdf", "asdf", 5);
    auto tested_st(sqllib::get_select_node_descr_in_box(db));
    tested_st.execute(5, 0, 0, 50, 50);
    tested_st.get_row(0);
}

BOOST_AUTO_TEST_CASE(lib_select_nodes_in_box)
{
    auto st0(sqllib::get_create_nodes_table(db, false));
    st0.execute();
    auto st1(sqllib::get_insert_node(db, false));
    st1.execute(10, 25, 40);
    auto st2(sqllib::get_create_toshow_table(db, false));
    st2.execute();
    auto st3(sqllib::get_create_node_attributes(db, false));
    st3.execute();
    auto st4(sqllib::get_insert_node_attr(db, false));
    st4.execute(10, "asdf", "asdf");
    auto st5(sqllib::get_insert_toshow(db, false));
    st5.execute("asdf", "asdf", 5);
    auto tested_st(sqllib::get_select_nodes_in_box(db));
    tested_st.execute(5, 0, 0, 50, 50);
    tested_st.get_row(0);
}

BOOST_AUTO_TEST_CASE(lib_select_way_descr_in_box)
{
    auto st0(sqllib::get_create_ways_table(db, false));
    st0.execute();
    auto st1(sqllib::get_create_nodes_table(db, false));
    st1.execute();
    auto st2(sqllib::get_create_edges_table(db, false));
    st2.execute();
    auto st3(sqllib::get_insert_way(db, false));
    st3.execute(41);
    auto st4(sqllib::get_insert_node(db, false));
    st4.execute(10, 25, 40);
    auto st5(sqllib::get_insert_node(db, false));
    st5.execute(11, 20, 30);
    auto st6(sqllib::get_insert_edge(db, false));
    st6.execute(41, 10, 11);
    auto st7(sqllib::get_create_toshow_table(db, false));
    st7.execute();
    auto st8(sqllib::get_create_way_attributes(db, false));
    st8.execute();
    auto st9(sqllib::get_insert_way_attr(db, false));
    st9.execute(41, "asdf", "asdf");
    auto st10(sqllib::get_insert_toshow(db, false));
    st10.execute("asdf", "asdf", 5);
    auto tested_st(sqllib::get_select_way_descr_in_box(db));
    tested_st.execute(5, 0, 0, 50, 50);
    tested_st.get_row(0);
}

BOOST_AUTO_TEST_CASE(lib_test_select)
{
    auto st0(sqllib::get_create_test_table(db, false));
    st0.execute();
    auto st1(sqllib::get_insert_test_table(db, false));
    st1.execute(10, "asdf", 20000000000);
    auto tested_st(sqllib::get_test_select(db));
    tested_st.execute(10);
    tested_st.get_row(0);
}


BOOST_AUTO_TEST_SUITE_END()


