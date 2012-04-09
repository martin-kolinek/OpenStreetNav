/*
 * RoadEdge.h
 *
 *  Created on: Mar 4, 2012
 *      Author: martin
 */

#ifndef ROADEDGE_H_
#define ROADEDGE_H_

#include <cstdint>
#include "../elements/osmelements.h"

namespace roads
{

class RoadEdge
{
public:
    osm::Way const& get_way() const;
    osm::Way& get_way();
    int& get_start_seq_no();
    int const& get_start_seq_no() const;
    int& get_end_seq_no();
    int const& get_end_seq_no() const;
    RoadEdge(osm::Way const& way, int start_seq_no, int end_seq_no, bool forward, double cost);
    virtual ~RoadEdge();
    osm::Way way;
    int start_seq_no, end_seq_no;
    bool forward;
    double cost;
};

} /* namespace roads */
#endif /* ROADEDGE_H_ */
