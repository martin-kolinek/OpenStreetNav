/*
 * NodeEdge.h
 *
 *  Created on: Mar 17, 2012
 *      Author: martin
 */

#ifndef NODEEDGE_H_
#define NODEEDGE_H_

#include "EdgeBase.h"
#include "Node.h"

namespace osm
{

class NodeEdge : public virtual EdgeBase
{
public:
    virtual Node& get_start_node() = 0;
    virtual Node& get_end_node() = 0;
    virtual Node const& get_start_node() const = 0;
    virtual Node const& get_end_node() const = 0;
    virtual ~NodeEdge();
};

} /* namespace osm */
#endif /* NODEEDGE_H_ */
