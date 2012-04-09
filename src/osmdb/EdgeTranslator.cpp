/*
 * EdgeTranslator.cpp
 *
 *  Created on: Apr 9, 2012
 *      Author: martin
 */

#include "EdgeTranslator.h"

namespace osmdb
{

std::vector<std::shared_ptr<display::Descriptible> > EdgeTranslator::translate(std::vector<osm::Edge> const&)
{
    return std::vector<std::shared_ptr<display::Descriptible> >();
}

EdgeTranslator::~EdgeTranslator()
{
}

} /* namespace osm */
