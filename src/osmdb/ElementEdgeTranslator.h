/*
 * ElementEdgeTranslator.h
 *
 *  Created on: Apr 9, 2012
 *      Author: martin
 */

#ifndef ELEMENTEDGETRANSLATOR_H_
#define ELEMENTEDGETRANSLATOR_H_

#include "EdgeTranslator.h"
#include "OsmDatabase.h"
#include "PropertiesSelection.h"

namespace osmdb
{

class ElementEdgeTranslator : public EdgeTranslator
{
public:
    ElementEdgeTranslator(OsmDatabase& db);
    std::vector<std::shared_ptr<display::Descriptible> > translate(std::vector<osm::Edge> const& v);
    virtual ~ElementEdgeTranslator();
private:
    osmdb::PropertiesSelection pdb;
};

} /* namespace osmdb */
#endif /* ELEMENTEDGETRANSLATOR_H_ */
