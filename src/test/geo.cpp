/*
 * geo.cpp
 *
 *  Created on: May 26, 2012
 *      Author: martin
 */


#include <boost/test/unit_test.hpp>
#include "../geoelements/geoelements.h"
#include <iomanip>

BOOST_AUTO_TEST_SUITE(geo)

BOOST_AUTO_TEST_CASE(conv_hull)
{
    std::vector<geo::Point> ps
    {
        geo::Point(0, 0),
        geo::Point(0, 1),
        geo::Point(1, 1),
        geo::Point(2, -1),
        geo::Point(1, 0),
        geo::Point(0.5, 0.5)
    };

    auto h = geo::get_convex_hull(ps);
    std::vector<geo::Point> right
    {
        geo::Point(0, 0),
        geo::Point(0, 1),
        geo::Point(1, 1),
        geo::Point(2, -1)
    };
    BOOST_CHECK(h == right);
}

BOOST_AUTO_TEST_CASE(conv_hull2)
{
    std::vector<geo::Point> ps
    {
        geo::Point(48.1481465, 17.1163054), //aaa
        /*geo::Point(48.1480818,17.1164011),
        geo::Point(48.1480818,17.1164011),
        geo::Point(48.1482014,17.1164916),*/
        geo::Point(48.1472602, 17.1194725), //aaa
        /*geo::Point(48.1470355,17.1188193),
        geo::Point(48.14749,17.1178541),
        geo::Point(48.1470355,17.1188193),
        geo::Point(48.1476991,17.1191001),
        geo::Point(48.1472602,17.1194725),
        geo::Point(48.14749,17.1178541),
        geo::Point(48.1479987,17.118423),*/
        geo::Point(48.1482503, 17.1164056), //aaa
        //geo::Point(48.1481465,17.1163054),
        geo::Point(48.1480818, 17.1164011),
        //geo::Point(48.1481465,17.1163054),
        //geo::Point(48.1482503,17.1164056),
        geo::Point(48.1482014, 17.1164916), //aaa
        /*geo::Point(48.1470355,17.1188193),
        geo::Point(48.14749,17.1178541),
        geo::Point(48.14749,17.1178541),
        geo::Point(48.1482014,17.1164916),
        geo::Point(48.1470355,17.1188193),*/
        geo::Point(48.1459997, 17.1158084), //aaa
        geo::Point(48.148255, 17.1200135), //aaa
        /*geo::Point(48.1476991,17.1191001),
        geo::Point(48.1476991,17.1191001),
        geo::Point(48.148255,17.1200135),
        geo::Point(48.1482014,17.1164916),
        geo::Point(48.1482503,17.1164056),
        geo::Point(48.1479987,17.118423),
        geo::Point(48.1476991,17.1191001)*/
    };

    /*std::vector<geo::Point> ps{
    	geo::Point(48.1459997,17.1158084),
    	geo::Point(48.1472602,17.1194725),
    	geo::Point(48.148255,17.1200135),
    	geo::Point(48.1482014,17.1164916),
    	geo::Point(48.1482503,17.1164056),
    	geo::Point(48.1481465,17.1163054)
    };*/

    auto h = geo::get_convex_hull(ps);
    BOOST_CHECK(h.size() == 5);
}


BOOST_AUTO_TEST_SUITE_END()

