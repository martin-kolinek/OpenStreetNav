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
        st = psql::Statement<psql::BindTypes<>, psql::RetTypes<> >("SET search_path TO testing", db);
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

BOOST_AUTO_TEST_CASE(lib_create_test_table)
{
    auto tested_st(sqllib::get_create_test_table(db));
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


