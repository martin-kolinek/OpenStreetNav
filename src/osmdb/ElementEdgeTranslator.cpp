/*
 * ElementEdgeTranslator.cpp
 *
 *  Created on: Apr 9, 2012
 *      Author: martin
 */

#include "ElementEdgeTranslator.h"
#include "../displayer/DescriptibleElement.h"

namespace osmdb
{

ElementEdgeTranslator::ElementEdgeTranslator(OsmDatabase& db):
    pdb(db)
{
}

std::vector<std::shared_ptr<display::Descriptible> > ElementEdgeTranslator::translate(std::vector<osm::Edge> const& v)
{
    std::vector<std::shared_ptr<display::Descriptible> > ret;
    for (auto it = v.begin(); it != v.end(); ++it)
    {
        std::shared_ptr<osm::Element> w(new osm::Way(it->get_way().id));
        w->fill(pdb);
        ret.push_back(std::shared_ptr<display::Descriptible>(new display::DescriptibleElement(w)));
    }
    return ret;
}

ElementEdgeTranslator::~ElementEdgeTranslator()
{
}

} /* namespace osm */
