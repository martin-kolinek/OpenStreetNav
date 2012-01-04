/*
 * Relation.cpp
 *
 *  Created on: Nov 5, 2011
 *      Author: martin
 */

#include "Relation.h"
#include "../osmdb/osmdb.h"
#include "Node.h"
#include "Way.h"

namespace osm
{

Relation::Relation()
{
}

Relation::Relation(int64_t id):
    id(id)
{
}

Relation::~Relation()
{
}

osm::ObjectType Relation::get_type() const
{
    return ObjectType::Relation;
}

bool Relation::operator==(const Element& e) const
{
    if (e.get_type() != ObjectType::Relation)
        return false;
    Relation const& r = static_cast<Relation const&>(e);
    if (r.id != id || r.tags != tags || r.members.size() != members.size())
        return false;
    for (auto it = members.begin(); it != members.end(); ++it)
    {
        bool wrong = true;
        for (auto it2 = r.members.lower_bound(it->first); it2 != r.members.upper_bound(it->first); ++it2)
        {
            if (*(it->second) == *(it->second))
                wrong = false;
        }
        if (wrong)
            return false;
    }
    return true;
}

boost::property_tree::ptree Relation::get_description()
{
    return boost::property_tree::ptree();
}

void Relation::fill(osmdb::PropertiesSelection& db)
{
    tags = db.get_relation_tags(id);
    auto m = db.get_node_members(id);
    for (auto it = m.begin(); it != m.end(); ++it)
    {
        members.insert(std::pair<std::string, std::shared_ptr<osm::Element> >(it->first, std::shared_ptr<osm::Element>(new osm::Node(it->second))));
    }
    m = db.get_way_members(id);
    for (auto it = m.begin(); it != m.end(); ++it)
    {
        members.insert(std::pair<std::string, std::shared_ptr<osm::Element> >(it->first, std::shared_ptr<osm::Element>(new osm::Way(it->second))));
    }
    m = db.get_relation_members(id);
    for (auto it = m.begin(); it != m.end(); ++it)
    {
        members.insert(std::pair<std::string, std::shared_ptr<osm::Element> >(it->first, std::shared_ptr<osm::Element>(new osm::Relation(it->second))));
    }
    for (auto it = members.begin(); it != members.end(); ++it)
    {
        it->second->fill(db);
    }
}

void Relation::add_to_relation(osmdb::ElementInsertion& db, int64_t relation, const std::string& role)
{
    db.insert_member_relation(relation, role, id);
}

void Relation::add_node(std::string const& role, const osm::Node& nd)
{
    add_member_ptr(role, new osm::Node(nd));
}

void Relation::add_way(std::string const& role, const osm::Way& w)
{
    add_member_ptr(role, new osm::Way(w));
}

void Relation::add_rel(std::string const& role, const osm::Relation& r)
{
    add_member_ptr(role, new osm::Relation(r));
}

void Relation::add_member_ptr(std::string const& role, osm::Element* ptr)
{
    members.insert(std::pair<std::string, std::shared_ptr<osm::Element> >(role, std::shared_ptr<osm::Element>(ptr)));
}

}
