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
#include "Element.h"
#include "Tag.h"

namespace osm
{

class Node : public Element
{
public:
    Node();
    Node(int64_t id, double lat, double lon);
    virtual ~Node();
    int64_t id;
    double lat;
    double lon;
    std::vector<Tag> tags;
    std::vector<Tag> const& get_tags();
    int64_t get_id();
    std::string get_type_str();
    bool operator==(Node const& other) const;
    bool operator!=(Node const& other) const;
};

}
#endif /* NODE_H_ */
