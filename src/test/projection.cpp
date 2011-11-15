/*
 * projection.cpp
 *
 *  Created on: Nov 12, 2011
 *      Author: martin
 */

#include <boost/test/unit_test.hpp>
#include <boost/test/floating_point_comparison.hpp>
#include "../projection/projection.h"

BOOST_AUTO_TEST_SUITE(projection)

BOOST_AUTO_TEST_CASE(some)
{
    proj::OrthoProjection proj(geo::Point(36, 50), 1);
    auto res = proj.project(36, 48);
    BOOST_CHECK_LT(res.x, 0);
    BOOST_CHECK_GT(res.y, 0);
    res = proj.project(37, 50);
    BOOST_CHECK(abs(res.x)< 0.0000001);
    BOOST_CHECK_GT(res.y, 0);
    proj = proj::OrthoProjection(geo::Point(-36, 50), 1);
    res = proj.project(-36, 48);
    BOOST_CHECK_LT(res.x, 0);
    BOOST_CHECK_LT(res.y, 0);
    res = proj.project(-37, 50);
    BOOST_CHECK(abs(res.x)< 0.0000001);
    BOOST_CHECK_LT(res.y, 0);
    proj = proj::OrthoProjection(geo::Point(-36, -50), 1);
    res = proj.project(-36, -48);
    BOOST_CHECK_GT(res.x, 0);
    BOOST_CHECK_LT(res.y, 0);
    res = proj.project(-37, -50);
    BOOST_CHECK(abs(res.x) < 0.0000001);
    BOOST_CHECK_LT(res.y, 0);
    proj = proj::OrthoProjection(geo::Point(36, -50), 1);
    res = proj.project(36, -48);
    BOOST_CHECK_GT(res.x, 0);
    BOOST_CHECK_GT(res.y, 0);
    res = proj.project(37, -50);
    BOOST_CHECK(abs(res.x)< 0.0000001);
    BOOST_CHECK_GT(res.y, 0);
}

BOOST_AUTO_TEST_CASE(border)
{
    proj::OrthoProjection proj(geo::Point(0, 0), 1);
    auto res = proj.project(90, 0);
    BOOST_CHECK(abs(res.x)< 0.0000001);
    BOOST_CHECK(abs(res.y - 1)< 0.0000001);
    res = proj.project(0, 90);
    BOOST_CHECK(abs(res.x - 1)< 0.0000001);
    BOOST_CHECK(abs(res.y)< 0.0000001);
    res = proj.project(-90, 0);
    BOOST_CHECK(abs(res.x)<0.0000001);
    BOOST_CHECK(abs(res.y + 1)< 0.0000001);
    res = proj.project(0, -90);
    BOOST_CHECK(abs(res.x + 1)< 0.0000001);
    BOOST_CHECK(abs(res.y)<0.0000001);
}

BOOST_AUTO_TEST_CASE(zero)
{
    proj::OrthoProjection proj(geo::Point(0, 0), 1);
    auto res = proj.project(0, 0);
    BOOST_CHECK(abs(res.x)< 0.0000001);
    BOOST_CHECK(abs(res.y)< 0.0000001);
    auto res2 = proj.unproject(0, 0);
    BOOST_CHECK(abs(res2.lat)< 0.0000001);
    BOOST_CHECK(abs(res2.lon)< 0.0000001);
    proj = proj::OrthoProjection(geo::Point(90, 0), 1);
    res = proj.project(90, 0);
    BOOST_CHECK(abs(res.x)<0.0000001);
    BOOST_CHECK(abs(res.y)< 0.0000001);
    proj = proj::OrthoProjection(geo::Point(45, 100), 1);
    res = proj.project(45, 100);
    BOOST_CHECK(abs(res.x)< 0.0000001);
    BOOST_CHECK(abs(res.y)< 0.0000001);
}

BOOST_AUTO_TEST_CASE(inverse)
{

    proj::OrthoProjection proj(geo::Point(-43, 65), 100);
    for (int i = -10; i <= 10; ++i)
    {
        for (int j = -10; j <= 10; ++j)
        {
            proj::FlatPoint p2 = proj.project(-43 + i, 65 + j);
            geo::Point p = proj.unproject(p2);
            BOOST_CHECK(abs(p.lat - (-43.0 + i)) <= 0.0000001);
            BOOST_CHECK(abs(p.lon - (65.0 + j)) <= 0.0000001);
            p = proj.unproject(i, j);
            p2 = proj.project(p);
            if (abs(p2.y - j) > 0)
            {
                std::cout << abs(p2.y - j) << std::endl;
            }
            BOOST_CHECK(abs(p2.x - i) <= 0.0000001);
            BOOST_CHECK(abs(p2.y - j) <= 0.0000001);
        }
    }
}

BOOST_AUTO_TEST_SUITE_END()
