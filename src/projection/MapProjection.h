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

/**
 * \class MapProjection
 * An interface which represents a projection of geospatial coordinates to planar coordinates.
 */
class MapProjection
{
public:
    /**
     * Project geospatial point onto plane
     * @param lat latitude of projected point
     * @param lon longitude of projected point
     * @return projection of the point
     */
    virtual FlatPoint project(double lat, double lon) = 0;
    /**
     * Project geospatial point onto plane
     * @param p point to project
     * @return projection of the point
     */
    virtual FlatPoint project(geo::Point const& p);
    /**
     * Transform point on a plane to a geospatial point that would be projected on it.
     * @param x the x coordinate of point
     * @param y the y coordinate of point
     * @return geospatial point
     */
    virtual geo::Point unproject(double x, double y) = 0;
    /**
     * Transform point on a plane to a geospatial point that would be projected on it.
     * @param p the point
     * @return geospatial point
     */
    virtual geo::Point unproject(FlatPoint const& p);
    virtual ~MapProjection();
};

} /* namespace geo */
#endif /* MAPPROJECTION_H_ */
