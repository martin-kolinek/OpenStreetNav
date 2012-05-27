/*
 * geoelements.h
 *
 *  Created on: Nov 12, 2011
 *      Author: martin
 */

#ifndef GEOELEMENTS_H_
#define GEOELEMENTS_H_

#include "Point.h"
#include "Edge.h"
#include <cmath>
#include <vector>
#include <set>
#include "../util/util.h"
#include <iostream>

namespace geo
{

double ccw(Point const& p1, Point const& p2, Point const& p3);

template<typename Rng>
std::vector<Point> get_convex_hull(Rng const& rng)
{
    class PointAngle
    {
    private:
        double val;
        bool only_x;
    public:
        Point const& p;
        PointAngle(Point const& orig, Point const& p):
            only_x(false),
            p(p)
        {
            if (std::abs(p.lat - orig.lat) < 0.0000001)
            {
                only_x = true;
                val = p.lon - orig.lon;
            }
            else
            {
                val = - (p.lon - orig.lon) / (p.lat - orig.lat);
            }
        }

        bool operator==(PointAngle const& other) const
        {
            return only_x == other.only_x && std::abs(other.val - val) < 0.00000001;
        }

        bool operator!=(PointAngle const& other) const
        {
            return util::not_eq_impl(*this, other);
        }

        bool operator<(PointAngle const& other) const
        {
            if (only_x && !other.only_x)
                return true;
            if (!only_x && other.only_x)
                return false;
            return (val < other.val);
        }

        bool operator<=(PointAngle const& other) const
        {
            return util::less_eq_impl(*this, other);
        }

        bool operator>(PointAngle const& other) const
        {
            return util::greater_than_impl(*this, other);
        }

        bool operator>=(PointAngle const& other) const
        {
            return util::greater_eq_impl(*this, other);
        }
    };

    auto it = rng.begin();
    if (it == rng.end())
        return std::vector<Point>();
    auto p = *it;
    for (; it != rng.end(); ++it)
    {
        if (*it < p)
        {
            p = *it;
        }
    }

    std::set<PointAngle> set;
    for (it = rng.begin(); it != rng.end(); ++it)
    {
        set.insert(PointAngle(p, *it));

    }

    std::vector<Point> ret;
    auto it2 = set.begin();
    while (it2 != set.end())
    {
        size_t s = ret.size();
        if (s < 2 || ccw(ret[s - 2], ret[s - 1], it2->p) > 0)
        {
            ret.push_back(it2->p);
            ++it2;
        }
        else
        {
            ret.pop_back();
        }
    }

    return ret;
}

}
#endif /* GEOELEMENTS_H_ */
