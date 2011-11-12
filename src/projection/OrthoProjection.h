/*
 * OrthoProjection.h
 *
 *  Created on: Nov 12, 2011
 *      Author: martin
 */

#ifndef ORTHOPROJECTION_H_
#define ORTHOPROJECTION_H_

#include "MapProjection.h"

namespace proj
{

/**
 * \class OrthoProjection
 * Class for orthographic projection of ball surface.
 * All angles are in degrees.
 */
class OrthoProjection : public MapProjection
{
public:
    OrthoProjection(geo::Point center, double radius);
    FlatPoint project(double lat, double lon);
    FlatPoint project(geo::Point const& p);
    geo::Point unproject(double x, double y);
    geo::Point unproject(FlatPoint const& p);
    virtual ~OrthoProjection();
private:
    double lt, ln, r;
    double deg(double rad);
    double rad(double deg);
};

} /* namespace geo */
#endif /* ORTHOPROJECTION_H_ */
