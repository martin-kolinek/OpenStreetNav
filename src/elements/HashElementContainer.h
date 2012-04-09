/*
 * WayElementContainer.h
 *
 *  Created on: Apr 9, 2012
 *      Author: martin
 */

#ifndef WAYELEMENTCONTAINER_H_
#define WAYELEMENTCONTAINER_H_

#include "ElementContainer.h"

namespace osm
{

class HashElementContainer : public ElementContainer
{
public:
    virtual int64_t get_way_id() const;
    virtual ~HashElementContainer();
};

} /* namespace osm */
#endif /* WAYELEMENTCONTAINER_H_ */
