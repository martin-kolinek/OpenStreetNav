/*
 * Node.h
 *
 *  Created on: Nov 5, 2011
 *      Author: martin
 */

#ifndef NODE_H_
#define NODE_H_

#include <stdint.h>
#include <map>
#include "Element.h"
#include "../geoelements/geoelements.h"

namespace osm
{

class Node : public Element
{
public:
    Node();
    Node(int64_t id, double lat = 0, double lon = 0);
    virtual ~Node();
    int64_t id;
    geo::Point position;
    std::multimap<std::string, std::string> tags;
    boost::property_tree::ptree get_description();
    osm::ObjectType get_type() const;
    bool operator==(Element const& e) const;
    void fill(osmdb::PropertiesSelection& db);
    void add_to_relation(osmdb::ElementInsertion& db, int64_t relation, std::string const& role);
    bool operator==(Node const& other) const;
    bool operator!=(Node const& other) const;
};

}
#endif /* NODE_H_ */
