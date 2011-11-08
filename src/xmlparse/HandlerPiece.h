/*
 * NodeHandlerPiece.h
 *
 *  Created on: Nov 8, 2011
 *      Author: martin
 */

#ifndef NODEHANDLERPIECE_H_
#define NODEHANDLERPIECE_H_

#include "ParserPiece.h"
#include "../elements/osmelements.h"

namespace osmxml
{

class HandlerPiece : public ParserPiece
{
public:
    virtual void handle_node(osm::Node const& nd) = 0;
    virtual void handle_way(osm::Way const& w) = 0;
    virtual void handle_relation(osm::Relation const& r) = 0;
    virtual ~HandlerPiece();
};

} /* namespace osmxml */
#endif /* NODEHANDLERPIECE_H_ */
