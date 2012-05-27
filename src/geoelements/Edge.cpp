/*
 * Edge.cpp
 *
 *  Created on: Nov 28, 2011
 *      Author: martin
 */

#include "Edge.h"
#include "../util/util.h"

namespace geo
{

geo::Edge::Edge(const Point& st, const Point& en):
    start(st),
    end(en)
{
}

bool geo::Edge::operator ==(const Edge& other) const
{
    return start == other.start && end == other.end;
}

bool geo::Edge::operator !=(const Edge& other) const
{
    return !(*this == other);
}

bool geo::Edge::operator<(Edge const& other) const
{
    return *this != other && std::make_pair(start, end) < std::make_pair(other.start, other.end);
}
bool geo::Edge::operator>(Edge const& other) const
{
    return util::greater_than_impl(*this, other);
}
bool geo::Edge::operator<=(Edge const& other) const
{
    return util::less_eq_impl(*this, other);
}
bool geo::Edge::operator>=(Edge const& other) const
{
    return util::greater_eq_impl(*this, other);
}

}

/* namespace geo */
