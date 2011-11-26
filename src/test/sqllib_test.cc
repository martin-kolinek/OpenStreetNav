/*
 * test_sqllib.cc
 *
 *  Created on: Nov 23, 2011
 *      Author: martin
 */

#include <boost/test/unit_test.hpp>
#include "../sqllib/sqllib.h"
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

BOOST_AUTO_TEST_CASE(lib_insert_test_table)
{
    auto st0(sqllib::get_create_test_table(db, false));
    st0.execute();
    auto tested_st(sqllib::get_insert_test_table(db));
    tested_st.execute(10, "gda", 34422354323);
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


