/*
 * Node.h
 *
 *  Created on: Nov 5, 2011
 *      Author: martin
 */

#ifndef NODE_H_
#define NODE_H_

#include <stdint.h>
#include <set>
#include "Element.h"
#include "../geoelements/geoelements.h"

namespace osm
{

/**
 * \class Node
 * Represents Node in OpenStreetMap (a point on map)
 */
class Node : public Element
{
public:
    Node();
    /**
     * Constructs a node with given id and position
     * @param id
     * @param lat latitude
     * @param lon longitude
     */
    Node(int64_t id, double lat = 0, double lon = 0);
    virtual ~Node();
    int64_t id;
    /**
     * Position of this Node.
     */
    geo::Point position;
    /**
     * Attributes of this Node.
     */
    std::set<osm::Tag> tags;
    boost::property_tree::ptree get_description() const;
    osm::ObjectType get_type() const;
    void fill(osmdb::PropertiesSelection& db);
    void add_to_relation(osmdb::ElementImporter& db, int64_t relation, std::string const& role);
    bool operator==(Node const& other) const;
    bool operator!=(Node const& other) const;
    virtual int64_t get_id() const;
};

}
#endif /* NODE_H_ */
