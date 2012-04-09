/*
 * DescriptibleElement.h
 *
 *  Created on: Apr 9, 2012
 *      Author: martin
 */

#ifndef DESCRIPTIBLEELEMENT_H_
#define DESCRIPTIBLEELEMENT_H_

#include "../elements/osmelements.h"
#include "Descriptible.h"

namespace display
{

class DescriptibleElement : public Descriptible
{
public:
    DescriptibleElement(std::shared_ptr<osm::Element> const& el);
    boost::property_tree::ptree get_description() const;
    std::shared_ptr<osm::HashElementContainer> get_highlighted() const;
    virtual ~DescriptibleElement();
private:
    std::shared_ptr<osm::Element> el;
};

} /* namespace display */
#endif /* DESCRIPTIBLEELEMENT_H_ */
