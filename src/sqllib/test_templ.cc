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

//$$

BOOST_AUTO_TEST_SUITE_END()


