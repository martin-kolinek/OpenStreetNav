/*
 * Node.h
 *
 *  Created on: Nov 5, 2011
 *      Author: martin
 */

#ifndef NODE_H_
#define NODE_H_

#include <stdint.h>
#include <vector>
#include "Tag.h"

namespace osm
{

class Node
{
public:
    Node(int64_t id);
    virtual ~Node();
    int64_t id;
    std::vector<Tag> tags;
};

}
#endif /* NODE_H_ */
