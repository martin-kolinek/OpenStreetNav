#include <boost/test/unit_test.hpp>

#include "../psql/psql.h"
#include "../sqllib/sqllib.h"
#include <tuple>

class PSqlFixture
{
public:
    psql::Database db;
    PSqlFixture():
        db("")
    {
        psql::Statement<psql::BindTypes<>, psql::RetTypes<> > st("CREATE SCHEMA testing", db);
        st.execute();
        st = psql::Statement<psql::BindTypes<>, psql::RetTypes<> >("SET search_path TO testing", db);
        st.execute();
    }
    ~PSqlFixture()
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

BOOST_FIXTURE_TEST_SUITE(psql2, PSqlFixture)

BOOST_AUTO_TEST_CASE(statement)
{
    auto st1(sqllib::get_create_test_table(db));
    st1.execute();
    auto st2(sqllib::get_insert_test_table(db));
    st2.execute(32, "asdf", 52341093);
    auto st3(sqllib::get_test_select(db));
    st3.execute(32);
    auto res(st3.get_row(0));
    std::tuple<int, std::string, int64_t> tup(32, "asdf", 52341093);
    BOOST_CHECK(res == tup);
}

BOOST_AUTO_TEST_SUITE_END()
