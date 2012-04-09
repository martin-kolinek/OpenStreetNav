/*
 * CompositeElementContainer.cpp
 *
 *  Created on: Apr 9, 2012
 *      Author: martin
 */

#include "CompositeElementContainer.h"

namespace osm
{

CompositeElementContainer::~CompositeElementContainer()
{
}

void CompositeElementContainer::add_way_container(std::shared_ptr<HashElementContainer> ptr)
{
    auto i = ptr->get_way_id();
    way_map[i] = ptr;
}

bool CompositeElementContainer::intersects(Edge const& el) const
{
    auto it = way_map.lower_bound(el.get_way().id);
    auto it2 = way_map.upper_bound(el.get_way().id);
    for (; it != it2; ++it)
    {
        if (it->second->intersects(el))
            return true;
    }
    return false;
}

void CompositeElementContainer::clear()
{
    way_map.clear();
}

} /* namespace osm */
