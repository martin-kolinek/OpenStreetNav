/*
 * Point.h
 *
 *  Created on: Nov 12, 2011
 *      Author: martin
 */

#ifndef POINT_H_
#define POINT_H_

#define EARTH_RADIUS 6372.8

namespace geo
{

/**
 * \class Point
 * Represents geospatial point with latitude and longitude specified in degrees.
 */
struct Point
{
public:
    Point(double lat = 0, double lon = 0);
    double lat;
    double lon;
    /**
     * same as close(other, 0.0000001)
     * @param other
     * @return
     */
    bool operator==(Point const& other) const;
    bool operator!=(Point const& other) const;
    bool operator<=(Point const& other) const;
    bool operator>=(Point const& other) const;
    bool operator>(Point const& other) const;
    /**
     * lexicographical ordering of points (used only to be able to store them in sets)
     * @param other
     * @return
     */
    bool operator<(Point const& other) const;
    bool before(Point const& other, double tolerance) const;
    bool close(Point const& other, double tolerance) const;

    double angle_distance(Point const& other) const;
};

double get_point_distance(double radius, Point const& p1, Point const& p2);

} /* namespace geo */
#endif /* POINT_H_ */
