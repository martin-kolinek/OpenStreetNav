/*
 * Point.cpp
 *
 *  Created on: Nov 12, 2011
 *      Author: martin
 */

#include "Point.h"
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
    return lat == other.lat && lon == other.lon;
}

bool Point::operator !=(const Point& other) const
{
    return !(*this == other);
}

bool Point::close(Point const& other, double tolerance) const
{
    return std::abs(lat - other.lat) < tolerance && std::abs(lon - other.lon) < tolerance;
}

} /* namespace geo */
