#include <cstdio>
#include "../sqlite/Statement.h"
#include "../sqlite/Database.h"
#include "../sqlite/SqliteException.h"
#include "../sqlite/InvalidUseException.h"
#include <boost/test/unit_test.hpp>
#include <boost/filesystem.hpp>
#include <iostream>
#include <vector>
#include <exception>

void cleanup()
{
    BOOST_CHECK_MESSAGE(boost::filesystem::exists("test.db"), "test.db does not exist");
    remove("test.db");
}

sqlite::Database get_test_db()
{
    BOOST_REQUIRE_MESSAGE(!boost::filesystem::exists("test.db"), "test.db exists");
    return sqlite::Database("test.db");
}

void prepare_test_db(sqlite::Database& db)
{
    sqlite::Statement st("CREATE TABLE test (ID INTEGER)", db);
    BOOST_REQUIRE_MESSAGE(!st.has_row(), "st has row");
    BOOST_REQUIRE_MESSAGE(!st.done(), "st is done");
    st.step();
    BOOST_REQUIRE_MESSAGE(!st.has_row(), "st has row2");
    BOOST_REQUIRE_MESSAGE(st.done(), "st is not done");
    st = sqlite::Statement("CREATE TABLE empty(ID INTEGER PRIMARY KEY)", db);
    st.step();
    st = sqlite::Statement("CREATE TABLE empty2(ID INTEGER PRIMARY KEY, A REAL)", db);
    st.step();
}
void insert_data(sqlite::Database& db)
{
    sqlite::Statement st("INSERT INTO test (ID) VALUES (1)", db);
    st.step();
    st = sqlite::Statement("INSERT INTO test (ID) VALUES (2)", db);
    st.step();
    st = sqlite::Statement("INSERT INTO test (ID) VALUES (3)", db);
    st.step();
}

class SimpleFixture
{
public:
    sqlite::Database db;
    SimpleFixture():
        db(get_test_db())
    {
        try
        {
            prepare_test_db(db);
            insert_data(db);
        }
        catch (...)
        {
            cleanup();
            throw;
        }
    }
    ~SimpleFixture()
    {
        cleanup();
    }
};

BOOST_FIXTURE_TEST_SUITE(SqliteSimpleTests, SimpleFixture)

BOOST_AUTO_TEST_CASE(fixture)
{
}

BOOST_AUTO_TEST_CASE(basic_usage)
{
    sqlite::Statement st("SELECT ID FROM test ORDER BY ID", db);
    st.step();
    BOOST_REQUIRE_MESSAGE(st.has_row(), "not has row");
    BOOST_REQUIRE_MESSAGE(!st.done(), "is done");
    BOOST_CHECK_EQUAL(1, st.val_int(0));
    st.step();
    BOOST_REQUIRE(!st.done());
    BOOST_REQUIRE(st.has_row());
    BOOST_CHECK_EQUAL(2, st.val_int(0));
    st.reset();
    BOOST_REQUIRE(!st.done());
    BOOST_REQUIRE(!st.has_row());
    std::vector<int> exp {1, 2, 3};
    std::vector<int> vals;
    while (!st.done())
    {
        st.step();
        if (st.has_row())
            vals.push_back(st.val_int(0));
    }
    BOOST_CHECK_MESSAGE(vals == exp, "vals == exp");
    BOOST_REQUIRE_MESSAGE(!st.has_row(), "has row 3");
    BOOST_REQUIRE_MESSAGE(st.done(), "not done 2");
}

BOOST_AUTO_TEST_CASE(sqlite_exception)
{
    BOOST_CHECK_THROW(sqlite::Statement st("SELECT ID FROM asdf", db), sqlite::SqliteException);
}

BOOST_AUTO_TEST_CASE(invalid_use_exception)
{
    sqlite::Statement st("SELECT ID FROM test", db);
    st.step();
    BOOST_CHECK_THROW(st.val_int(2), sqlite::InvalidUseException);
    BOOST_CHECK_THROW(st.val_int(-1), sqlite::InvalidUseException);
}

BOOST_AUTO_TEST_CASE(step_failure)
{
    {
        sqlite::Statement st1("INSERT INTO empty (ID) VALUES(1)", db);
        st1.step();
        st1.reset();
        BOOST_REQUIRE_THROW(st1.step(), sqlite::SqliteException);
        BOOST_CHECK(!st1.done());
        BOOST_CHECK(!st1.has_row());
        sqlite::Statement st2("DELETE FROM empty", db);
        st2.step();
        BOOST_CHECK_NO_THROW(st1.step());
        st2 = sqlite::Statement("SELECT ID FROM empty", db);
        st2.step();
        BOOST_CHECK(st2.has_row());
    }
    BOOST_CHECK_EQUAL(0, db.unfinalized());
}

BOOST_AUTO_TEST_CASE(bind)
{
    sqlite::Statement st("INSERT INTO empty2 (ID, A) VALUES(?,?)", db);
    st.bind_int(1, 20);
    st.bind_double(2, 3);
    BOOST_CHECK_THROW(st.bind_double(0, 19), sqlite::SqliteException);
    BOOST_CHECK_THROW(st.bind_double(3, 29), sqlite::SqliteException);
    st.step();
    sqlite::Statement st2("SELECT ID, A FROM empty2", db);
    st2.step();
    BOOST_CHECK(st2.has_row());
    BOOST_CHECK_EQUAL(st2.val_int(0), 20);
    BOOST_CHECK_CLOSE(st2.val_double(1), 3, 0.0001);
    st2.step();
    BOOST_CHECK(st2.done());
}

BOOST_AUTO_TEST_SUITE_END()