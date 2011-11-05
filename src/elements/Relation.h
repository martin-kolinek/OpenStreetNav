/*
 * Relation.h
 *
 *  Created on: Nov 5, 2011
 *      Author: martin
 */

#ifndef RELATION_H_
#define RELATION_H_

#include "RelationMapping.h"
#include "Tag.h"
#include <vector>

namespace osm
{

class Relation
{
public:
    Relation(int64_t id);
    virtual ~Relation();
    int64_t id;
    std::vector<RelationMapping> members;
    std::vector<Tag> tags;
};

}
#endif /* RELATION_H_ */