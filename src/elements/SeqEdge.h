/*
 * SeqEdge.h
 *
 *  Created on: Mar 17, 2012
 *      Author: martin
 */

#ifndef SEQEDGE_H_
#define SEQEDGE_H_

#include "EdgeBase.h"

namespace osm
{

class SeqEdge : public virtual EdgeBase
{
public:
    virtual ~SeqEdge();
    virtual int& get_start_seq_no() = 0;
    virtual int const& get_start_seq_no() const = 0;
    virtual int& get_end_seq_no() = 0;
    virtual int const& get_end_seq_no() const = 0;
};

} /* namespace osm */
#endif /* SEQEDGE_H_ */
