/*
 * Relation.h
 *
 *  Created on: Nov 5, 2011
 *      Author: martin
 */

#ifndef RELATION_H_
#define RELATION_H_

#include <memory>
#include <map>
#include <set>
#include "Element.h"

namespace osm
{

class Node;
class Way;

class Relation : public Element
{
public:
    Relation();
    Relation(int64_t id);
    virtual ~Relation();
    int64_t id;
    std::multimap<std::string, std::shared_ptr<osm::Element> > members;
    std::set<osm::Tag> tags;
    boost::property_tree::ptree get_description();
    osm::ObjectType get_type() const;
    bool operator==(Relation const& r) const;
    bool operator!=(Relation const& r) const;
    void fill(osmdb::PropertiesSelection& db);
    void add_to_relation(osmdb::ElementImporter& db, int64_t relation, std::string const& role);
    virtual int64_t get_id() const;
    void add_node(std::string const& role, osm::Node const& nd);
    void add_way(std::string const& role, osm::Way const& w);
    void add_rel(std::string const& role, osm::Relation const& r);
private:
    void add_member_ptr(std::string const& role, osm::Element* ptr);
};

}
#endif /* RELATION_H_ */
