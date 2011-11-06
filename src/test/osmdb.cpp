/*
 * osmdb.cpp
 *
 *  Created on: Nov 6, 2011
 *      Author: martin
 */

#include <boost/test/unit_test.hpp>
#include <boost/filesystem.hpp>
#include "../osmdb/OsmDatabase.h"
#include "../osmdb/WrongDBException.h"

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

BOOST_AUTO_TEST_SUITE_END()
