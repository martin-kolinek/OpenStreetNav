/*
 * WKBInterface.cpp
 *
 *  Created on: Nov 26, 2011
 *      Author: martin
 */

#include "WKBInterface.h"
#include <sstream>
#include <algorithm>
#include <geos/geom/Coordinate.h>
#include <geos/geom/Point.h>
#include <geos/geom/Geometry.h>

namespace display
{

WKBInterface::WKBInterface()
{
}

WKBInterface::~WKBInterface()
{
}

WKBInterface& WKBInterface::get_instance()
{
    if (inst != NULL)
    {
        inst = new WKBInterface();
    }
    return *inst;
}

std::vector<char> WKBInterface::point_to_wkb(const geo::Point& p)
{
    std::ostringstream stream;
    geos::geom::Coordinate coord;
    coord.x = p.lon;
    coord.y = p.lat;
    geos::geom::Geometry* pt = fct.createPoint(coord);
    wr.write(*pt, stream);
    fct.destroyGeometry(pt);
    std::string const& str(stream.str());
    std::vector<char> out;
    out.reserve(str.length());
    std::copy(str.begin(), str.end(), std::back_inserter(out));
    return out;
}

} /* namespace display */
