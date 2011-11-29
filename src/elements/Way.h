/*
 * Way.h
 *
 *  Created on: Nov 5, 2011
 *      Author: martin
 */

#ifndef WAY_H_
#define WAY_H_

#include <stdint.h>
#include "Tag.h"
#include "Element.h"
#include <vector>

namespace osm
{

class Way : public Element
{
public:
    Way();
    Way(int64_t id);
    virtual ~Way();
    int64_t id;
    std::vector<int64_t> nodes;
    std::vector<Tag> tags;
    std::vector<Tag> const& get_tags();
    int64_t get_id();
    std::string get_type_str();
    bool operator==(Way const& other) const;
    bool operator!=(Way const& other) const;
};

}
#endif /* WAY_H_ */
