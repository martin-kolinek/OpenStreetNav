/*
 * RoadEdge.h
 *
 *  Created on: Mar 4, 2012
 *      Author: martin
 */

#ifndef ROADEDGE_H_
#define ROADEDGE_H_

#include <cstdint>

namespace roads
{

class RoadEdge
{
public:
    RoadEdge(int64_t way_id, int start_seq_no, int end_seq_no, bool forward, double cost);
    virtual ~RoadEdge();
    int64_t way_id;
    int start_seq_no, end_seq_no;
    bool forward;
    double cost;
};

} /* namespace roads */
#endif /* ROADEDGE_H_ */
