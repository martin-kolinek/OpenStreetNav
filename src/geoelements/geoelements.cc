/*
 * geoelements.cc
 *
 *  Created on: May 25, 2012
 *      Author: martin
 */

#include "geoelements.h"

namespace geo
{

double ccw(Point const& p1, Point const& p2, Point const& p3)
{
    return (p2.lon - p1.lon) * (p3.lat - p1.lat) - (p2.lat - p1.lat) * (p3.lon - p1.lon);
}

}
