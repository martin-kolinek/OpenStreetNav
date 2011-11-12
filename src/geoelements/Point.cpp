/*
 * Point.cpp
 *
 *  Created on: Nov 12, 2011
 *      Author: martin
 */

#include "Point.h"

namespace geo
{

Point::Point(double lat, double lon):
    lat(lat),
    lon(lon)
{
}

bool Point::operator ==(const Point& other)
{
    return lat == other.lat && lon == other.lon;
}

bool Point::operator !=(const Point& other)
{
    return !(*this == other);
}

} /* namespace geo */
