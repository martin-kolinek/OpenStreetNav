/*
 * EdgeBase.h
 *
 *  Created on: Mar 17, 2012
 *      Author: martin
 */

#ifndef EDGEBASE_H_
#define EDGEBASE_H_

#include "Way.h"

namespace osm
{

class EdgeBase
{
public:
    virtual ~EdgeBase();
    virtual osm::Way const& get_way() const = 0;
    virtual osm::Way& get_way() = 0;
};

} /* namespace osm */
#endif /* EDGEBASE_H_ */
