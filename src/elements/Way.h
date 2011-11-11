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
#include <vector>

namespace osm
{

class Way
{
public:
	Way();
    Way(int64_t id);
    virtual ~Way();
    int64_t id;
    std::vector<int64_t> nodes;
    std::vector<Tag> tags;
    bool operator==(Way const& other) const;
    bool operator!=(Way const& other) const;
};

}
#endif /* WAY_H_ */
