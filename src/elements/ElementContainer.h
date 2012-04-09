/*
 * ElementContainer.h
 *
 *  Created on: Apr 9, 2012
 *      Author: martin
 */

#ifndef ELEMENTCONTAINER_H_
#define ELEMENTCONTAINER_H_

#include "ContainedElement.h"
#include "Edge.h"

namespace osm
{

class ElementContainer
{
public:
    virtual bool intersects(Edge const& el) const;
    virtual ~ElementContainer();
};

} /* namespace osm */
#endif /* ELEMENTCONTAINER_H_ */
