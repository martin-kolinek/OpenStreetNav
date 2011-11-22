#include <boost/test/unit_test.hpp>

#include "../psql/psql.h"

BOOST_AUTO_TEST_SUITE(psql)

BOOST_AUTO_TEST_CASE(db)
{
    psql::Database db("");
    db.get_db();
}

BOOST_AUTO_TEST_CASE(sync)
{
    psql::Database db("", true);
    db.get_db();
}

BOOST_AUTO_TEST_CASE(dbfail)
{
    BOOST_CHECK_THROW(psql::Database db("asdf"), psql::PgSqlException);
}

BOOST_AUTO_TEST_CASE(dbfail2)
{
    BOOST_CHECK_THROW(psql::Database("user=asdfgasdafgasdghafasasa", true), psql::PgSqlException);
}

BOOST_AUTO_TEST_CASE(dbfailasync2)
{
    psql::Database db("user=asdfgasdafgasdghafasasa", false);
    BOOST_CHECK_THROW(db.get_db(), psql::PgSqlException);
}

BOOST_AUTO_TEST_SUITE_END()
