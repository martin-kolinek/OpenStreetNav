/*
 * MapProjection.h
 *
 *  Created on: Nov 12, 2011
 *      Author: martin
 */

#ifndef MAPPROJECTION_H_
#define MAPPROJECTION_H_

#include "../geoelements/geoelements.h"
#include "FlatPoint.h"

namespace proj
{

class MapProjection
{
public:
    virtual FlatPoint project(double lat, double lon) = 0;
    virtual FlatPoint project(geo::Point const& p);
    virtual geo::Point unproject(double x, double y) = 0;
    virtual geo::Point unproject(FlatPoint const& p);
    virtual ~MapProjection();
};

} /* namespace geo */
#endif /* MAPPROJECTION_H_ */
