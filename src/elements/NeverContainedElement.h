/*
 * NeverContainedElement.h
 *
 *  Created on: May 25, 2012
 *      Author: martin
 */

#ifndef NEVERCONTAINEDELEMENT_H_
#define NEVERCONTAINEDELEMENT_H_

#include "ContainedElement.h"

namespace osm
{

class NeverContainedElement : public ContainedElement
{
public:
    bool is_intersected(ElementContainer const& c) const;
    virtual ~NeverContainedElement();
};

} /* namespace osm */
#endif /* NEVERCONTAINEDELEMENT_H_ */
