/*
 * Way.h
 *
 *  Created on: Nov 5, 2011
 *      Author: martin
 */

#ifndef WAY_H_
#define WAY_H_

#include <stdint.h>
#include "Element.h"
#include "Node.h"
#include <vector>
#include <set>

namespace osm
{

/**
 * \class Way
 * Represents a way in OpenStreetMap (a sequence of Nodes)
 */
class Way : public Element
{
public:
    Way();
    /**
     * Constructs a Way with given id.
     * @param id
     */
    Way(int64_t id);
    virtual ~Way();
    int64_t id;
    /**
     * Member nodes - from first to last.
     */
    std::vector<osm::Node> nodes;
    /**
     * Way attributes.
     */
    std::set<osm::Tag> tags;
    boost::property_tree::ptree get_description() const;
    osm::ObjectType get_type() const;
    bool operator==(Way const& e) const;
    bool operator!=(Way const& e) const;
    void fill(osmdb::PropertiesSelection& db);
    virtual int64_t get_id() const;
    void add_to_relation(osmdb::ElementImporter& db, int64_t relation, std::string const& role);
};

}
#endif /* WAY_H_ */
