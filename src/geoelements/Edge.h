/*
 * Edge.h
 *
 *  Created on: Nov 28, 2011
 *      Author: martin
 */

#ifndef GEO_EDGE_H_
#define GEO_EDGE_H_

#include "Point.h"

namespace geo
{

class Edge
{
public:
    Edge(Point const& st, Point const& en);
    Point start;
    Point end;
    bool operator==(Edge const& other) const;
    bool operator!=(Edge const& other) const;
    bool operator<(Edge const& other) const;
    bool operator>(Edge const& other) const;
    bool operator<=(Edge const& other) const;
    bool operator>=(Edge const& other) const;
};

} /* namespace geo */
#endif /* EDGE_H_ */
