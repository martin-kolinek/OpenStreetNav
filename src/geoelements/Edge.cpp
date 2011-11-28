/*
 * Edge.cpp
 *
 *  Created on: Nov 28, 2011
 *      Author: martin
 */

#include "Edge.h"

namespace geo
{

geo::Edge::Edge(const Point& st, const Point& en):
    start(st),
    end(en)
{
}

bool geo::Edge::operator ==(const Edge& other)
{
    return start == other.start && end == other.end;
}

bool geo::Edge::operator !=(const Edge& other)
{
    return !(*this == other);
}

}

/* namespace geo */
