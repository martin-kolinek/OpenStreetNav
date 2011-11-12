/*
 * FlatPoint.cpp
 *
 *  Created on: Nov 12, 2011
 *      Author: martin
 */

#include "FlatPoint.h"

namespace proj
{

FlatPoint::FlatPoint(double x, double y):
    x(x),
    y(y)
{
}

bool FlatPoint::operator ==(const FlatPoint& other)
{
    return x == other.x && y == other.y;
}

bool FlatPoint::operator !=(const FlatPoint& other)
{
    return !(*this == other);
}

} /* namespace geo */
