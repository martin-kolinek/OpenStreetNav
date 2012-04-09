/*
 * EdgeTranslator.h
 *
 *  Created on: Apr 9, 2012
 *      Author: martin
 */

#ifndef EDGETRANSLATOR_H_
#define EDGETRANSLATOR_H_

#include "../displayer/Descriptible.h"
#include "../elements/osmelements.h"

namespace osmdb
{

class EdgeTranslator
{
public:
    virtual std::vector<std::shared_ptr<display::Descriptible> > translate(std::vector<osm::Edge> const&);
    virtual ~EdgeTranslator();
};

} /* namespace osmdb */
#endif /* EDGETRANSLATOR_H_ */
