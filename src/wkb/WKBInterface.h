/*
 * WKBInterface.h
 *
 *  Created on: Nov 26, 2011
 *      Author: martin
 */

#ifndef WKBINTERFACE_H_
#define WKBINTERFACE_H_

#include "../geoelements/geoelements.h"
#include <geos/geom/GeometryFactory.h>
#include <geos/io/WKBWriter.h>
#include <vector>

namespace display
{

class WKBInterface
{
public:
    WKBInterface();
    virtual ~WKBInterface();
    WKBInterface& get_instance();
    std::vector<char> point_to_wkb(geo::Point const& p);
private:
    static WKBInterface* inst;
    geos::io::WKBWriter wr;
    geos::geom::GeometryFactory fct;
};

} /* namespace display */
#endif /* WKBINTERFACE_H_ */
