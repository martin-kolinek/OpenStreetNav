/*
 * MapProjection.cpp
 *
 *  Created on: Nov 12, 2011
 *      Author: martin
 */

#include "MapProjection.h"

namespace proj
{

FlatPoint MapProjection::project(geo::Point const& p)
{
    return project(p.lat, p.lon);
}

geo::Point MapProjection::unproject(FlatPoint const& p)
{
    return unproject(p.x, p.y);
}

MapProjection::~MapProjection()
{
}

} /* namespace geo */
