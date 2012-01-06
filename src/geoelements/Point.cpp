/*
 * Point.cpp
 *
 *  Created on: Nov 12, 2011
 *      Author: martin
 */

#include "Point.h"
#include <cmath>
#include <utility>

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
    return !(*this == other);
}

bool Point::operator <=(const Point& other) const
{
    return *this == other || *this < other;
}

bool Point::operator >=(const Point& other) const
{
    return !(*this < other);
}

bool Point::operator >(const Point& other) const
{
    return *this != other && !(*this < other);
}

bool Point::operator <(const Point& other) const
{
    std::pair<double, double> t(lat, lon);
    std::pair<double, double> o(other.lat, other.lon);
    return *this != other && t < o;
}

bool Point::close(const Point& other, double tolerance) const
{
    return std::abs(lat - other.lat) < tolerance && std::abs(lon - other.lon) < tolerance;
}

} /* namespace geo */
