#define BOOST_TEST_MODULE OpenStreetNavTest
#include <boost/test/unit_test.hpp>
#include <tuple>
#include <functional>
#include "../util/make_ref.h"

BOOST_AUTO_TEST_CASE(always_success)
{
    BOOST_CHECK(true);
}
