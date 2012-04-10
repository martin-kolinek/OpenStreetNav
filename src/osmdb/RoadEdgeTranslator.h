/*
 * RoadEdgeTranslator.h
 *
 *  Created on: Apr 11, 2012
 *      Author: martin
 */

#ifndef ROADEDGETRANSLATOR_H_
#define ROADEDGETRANSLATOR_H_

#include "EdgeTranslator.h"
#include "RoadEdgeProperties.h"

namespace osmdb
{

class RoadEdgeTranslator : public EdgeTranslator
{
public:
    RoadEdgeTranslator(OsmDatabase& db);
    std::vector<std::shared_ptr<display::Descriptible> > translate(std::vector<osm::Edge> const&);
    virtual ~RoadEdgeTranslator();
private:
    RoadEdgeProperties prop;
};

} /* namespace osmdb */
#endif /* ROADEDGETRANSLATOR_H_ */
