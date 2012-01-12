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
#include <map>

namespace osm
{

class Way : public Element
{
public:
    Way();
    Way(int64_t id);
    virtual ~Way();
    int64_t id;
    std::vector<osm::Node> nodes;
    std::multimap<std::string, std::string> tags;
    boost::property_tree::ptree get_description();
    osm::ObjectType get_type() const;
    bool operator==(Way const& e) const;
    bool operator!=(Way const& e) const;
    void fill(osmdb::PropertiesSelection& db);
    virtual int64_t get_id() const;
    void add_to_relation(osmdb::ElementImporter& db, int64_t relation, std::string const& role);
};

}
#endif /* WAY_H_ */
