/*
 * Point.cpp
 *
 *  Created on: Nov 12, 2011
 *      Author: martin
 */

#include "Point.h"
#include <cmath>
#include <utility>
#include "../util.h"

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

} /* namespace geo */
