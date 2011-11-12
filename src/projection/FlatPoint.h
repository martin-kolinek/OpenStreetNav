/*
 * FlatPoint.h
 *
 *  Created on: Nov 12, 2011
 *      Author: martin
 */

#ifndef FLATPOINT_H_
#define FLATPOINT_H_

namespace proj
{

struct FlatPoint
{
public:
    FlatPoint(double x = 0, double y = 0);
    double x;
    double y;
    bool operator==(FlatPoint const& other);
    bool operator!=(FlatPoint const& other);
};

} /* namespace geo */
#endif /* FLATPOINT_H_ */
