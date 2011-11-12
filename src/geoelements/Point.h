/*
 * Point.h
 *
 *  Created on: Nov 12, 2011
 *      Author: martin
 */

#ifndef POINT_H_
#define POINT_H_

namespace geo
{

struct Point
{
public:
    Point(double lat = 0, double lon = 0);
    double lat;
    double lon;
    bool operator==(Point const& other);
    bool operator!=(Point const& other);
};

} /* namespace geo */
#endif /* POINT_H_ */