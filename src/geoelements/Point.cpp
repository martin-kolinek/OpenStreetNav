/*
 * Point.cpp
 *
 *  Created on: Nov 12, 2011
 *      Author: martin
 */

#include "Point.h"
#include <cmath>
#include <utility>
#include "../util/util.h"
#define _USE_MATH_DEFINES
#include <cmath>

namespace geo
{

Point::Point(double lat, double lon):
    lat(lat),
    lon(lon)
{
}

bool Point::operator ==(const Point& other) const
{
    return close(other, 0.0000001);
}

bool Point::operator !=(const Point& other) const
{
    return util::not_eq_impl(*this, other);
}

bool Point::operator <=(const Point& other) const
{
    return util::less_eq_impl(*this, other);
}

bool Point::operator >=(const Point& other) const
{
    return util::greater_eq_impl(*this, other);
}

bool Point::operator >(const Point& other) const
{
    return util::greater_than_impl(*this, other);
}

bool Point::operator <(const Point& other) const
{
    return before(other, 0.0000001);
}

bool Point::before(Point const& other, double tolerance) const
{
    if (std::abs(lat - other.lat) < tolerance)
    {
        return lon - other.lon < -tolerance;
    }
    return lat < other.lat;
}

bool Point::close(const Point& other, double tolerance) const
{
    return std::abs(lat - other.lat) < tolerance && std::abs(lon - other.lon) < tolerance;
}

double Point::angle_distance(Point const& other) const
{
    double d_lat = std::abs(lat - other.lat) * M_PI / 180.0;
    double d_lon = std::abs(lon - other.lon) * M_PI / 180.0;
    double a1 = std::sin(d_lat / 2.0);
    double a1s = a1 * a1;
    double a2 = std::sin(d_lon / 2.0);
    double a2s = a2 * a2;
    double a3 = std::cos(lat * M_PI / 180) * std::cos(other.lat * M_PI / 180);
    double d_ang = 2 * std::asin(std::sqrt(a1s + a2s * a3));
    return d_ang * 180.0 / M_PI;
}

double get_point_distance(double radius, Point const& p1, Point const& p2)
{
    double d = radius * (p1.angle_distance(p2) * M_PI / 180.0);
    return d;
}

} /* namespace geo */
