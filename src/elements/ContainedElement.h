/*
 * ContainedElement.h
 *
 *  Created on: Apr 9, 2012
 *      Author: martin
 */

#ifndef CONTAINEDELEMENT_H_
#define CONTAINEDELEMENT_H_

namespace osm
{

class ElementContainer;

class ContainedElement
{
public:
    virtual bool is_intersected(ElementContainer const& c) const = 0;
    virtual ~ContainedElement();
};

} /* namespace osm */
#endif /* CONTAINEDELEMENT_H_ */
