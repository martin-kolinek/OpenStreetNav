/*
 * DescriptibleElement.cpp
 *
 *  Created on: Apr 9, 2012
 *      Author: martin
 */

#include "DescriptibleElement.h"

namespace display
{

DescriptibleElement::DescriptibleElement(std::shared_ptr<osm::Element> const& el):
    el(el)
{

}

boost::property_tree::ptree DescriptibleElement::get_description() const
{
    return el->get_description();
}

std::shared_ptr<osm::HashElementContainer> DescriptibleElement::get_highlighted() const
{
    return el->get_highlighted();
}

DescriptibleElement::~DescriptibleElement()
{
}

} /* namespace display */
