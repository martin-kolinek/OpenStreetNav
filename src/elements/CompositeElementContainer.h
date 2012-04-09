/*
 * CompositeElementContainer.h
 *
 *  Created on: Apr 9, 2012
 *      Author: martin
 */

#ifndef COMPOSITEELEMENTCONTAINER_H_
#define COMPOSITEELEMENTCONTAINER_H_

#include "HashElementContainer.h"

namespace osm
{

class CompositeElementContainer : public ElementContainer
{
public:
    void add_way_container(std::shared_ptr<HashElementContainer> ptr);
    bool intersects(Edge const& el) const;
    void clear();
    virtual ~CompositeElementContainer();
private:
    std::map<int64_t, std::shared_ptr<ElementContainer> > way_map;
};

} /* namespace osm */
#endif /* COMPOSITEELEMENTCONTAINER_H_ */
