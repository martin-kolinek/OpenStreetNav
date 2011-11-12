/*
 * OrthoProjection.cpp
 *
 *  Created on: Nov 12, 2011
 *      Author: martin
 */

#include "OrthoProjection.h"
#include <cmath>

namespace proj
{

OrthoProjection::OrthoProjection(geo::Point center, double radius):
    lt(rad(center.lat)),
    ln(rad(center.lon)),
    r(radius)
{
}

FlatPoint OrthoProjection::project(double lat, double lon)
{
    lat = rad(lat);
    lon = rad(lon);
    double x = r * cos(lat) * sin(lon - ln);
    double y = r * (cos(lt) * sin(lat) - sin(lt) * cos(lat) * cos(lon - ln));
    return FlatPoint(x, y);
}

geo::Point OrthoProjection::unproject(double x, double y)
{
    double ro = sqrt(x * x + y * y);
    if (ro == 0)
        return geo::Point(deg(lt), deg(ln));
    double c = asin(ro / r);
    double lat = asin(cos(c) * sin(lt) + (y * sin(c) * cos(lt)) / ro);
    double lon = ln + atan2(x * sin(c), ro * cos(lt) * cos(c) - y * sin(lt) * sin(c));
    return geo::Point(deg(lat), deg(lon));
}

OrthoProjection::~OrthoProjection()
{
}

double OrthoProjection::deg(double rad)
{
    return rad * 180.0 / M_PI;
}

FlatPoint OrthoProjection::project(const geo::Point& p)
{
    return MapProjection::project(p);
}

geo::Point OrthoProjection::unproject(const FlatPoint& p)
{
    return MapProjection::unproject(p);
}

double OrthoProjection::rad(double deg)
{
    return deg * M_PI / 180.0;
}

} /* namespace geo */
