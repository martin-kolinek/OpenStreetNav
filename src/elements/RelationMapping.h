/*
 * RelationMapping.h
 *
 *  Created on: Nov 5, 2011
 *      Author: martin
 */

#ifndef RELATIONMAPPING_H_
#define RELATIONMAPPING_H_

#include "ObjectType.h"
#include <stdint.h>
#include <string>

namespace osm
{

class RelationMapping
{
public:
    RelationMapping();
    RelationMapping(std::string const& role, ObjectType type, int64_t id);
    virtual ~RelationMapping();
    std::string role;
    ObjectType type;
    int64_t id;
    bool operator==(RelationMapping const& other) const;
    bool operator!=(RelationMapping const& other) const;
};

}
#endif /* RELATIONMAPPING_H_ */
