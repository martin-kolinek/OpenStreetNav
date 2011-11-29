/*
 * Element.h
 *
 *  Created on: Nov 29, 2011
 *      Author: martin
 */

#ifndef OSMELEMENT_H_
#define OSMELEMENT_H_

#include "Tag.h"
#include <string>
#include <vector>
#include <stdint.h>

namespace osm
{

class Element
{
public:
    virtual ~Element();
    virtual std::vector<Tag> const& get_tags() = 0;
    virtual int64_t get_id() = 0;
    virtual std::string get_type_str() = 0;
};

} /* namespace osm */
#endif /* ELEMENT_H_ */
