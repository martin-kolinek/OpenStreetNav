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

bool Relation::operator==(const Relation& r) const
{
    return r.id == id && util::multimap_eq(tags, r.tags) &&
           util::multimap_eq < decltype(deref_eq_by_id(members.begin()->second, members.begin()->second)) > (members, r.members);
}

bool Relation::operator!=(const Relation& r) const
{
    return !(*this == r);
}

boost::property_tree::ptree Relation::get_description()
{
    boost::property_tree::ptree ret;
    boost::property_tree::ptree rel;
    rel.data() = util::to_str(id);
    boost::property_tree::ptree tags_desc;
    for (auto it = tags.begin(); it != tags.end(); ++it)
    {
        tags_desc.push_back(*it);
    }
    rel.put_child("tags", tags_desc);
    boost::property_tree::ptree mmbrs;
    for (auto it = members.begin(); it != members.end(); ++it)
    {
        boost::property_tree::ptree mem;
        mem.put("role", it->first);
        mem.push_back(it->second->get_description().front());
        mmbrs.add_child("member", mem);
    }
    rel.put_child("members", mmbrs);
    ret.put_child("relation", rel);
    return ret;
}

void Relation::fill(osmdb::PropertiesSelection& db)
{
    tags = db.get_relation_tags(id);
    auto m = db.get_node_members(id);
    for (auto it = m.begin(); it != m.end(); ++it)
    {
        members.insert(std::pair<std::string, std::shared_ptr<osm::Element> >(it->first, std::shared_ptr < osm::Element > (new osm::Node(it->second))));
    }
    m = db.get_way_members(id);
    for (auto it = m.begin(); it != m.end(); ++it)
    {
        members.insert(std::pair<std::string, std::shared_ptr<osm::Element> >(it->first, std::shared_ptr < osm::Element > (new osm::Way(it->second))));
    }
    m = db.get_relation_members(id);
    for (auto it = m.begin(); it != m.end(); ++it)
    {
        members.insert(std::pair<std::string, std::shared_ptr<osm::Element> >(it->first, std::shared_ptr < osm::Element > (new osm::Relation(it->second))));
    }
    for (auto it = members.begin(); it != members.end(); ++it)
    {
        it->second->fill(db);
    }
}

void Relation::add_to_relation(osmdb::ElementImporter& db, int64_t relation, const std::string& role)
{
    db.insert_member_relation(relation, role, id);
}

void Relation::add_node(const std::string& role, const osm::Node& nd)
{
    add_member_ptr(role, new osm::Node(nd));
}

void Relation::add_way(const std::string& role, const osm::Way& w)
{
    add_member_ptr(role, new osm::Way(w));
}

void Relation::add_rel(const std::string& role, const osm::Relation& r)
{
    add_member_ptr(role, new osm::Relation(r));
}

int64_t Relation::get_id() const
{
    return id;
}

void Relation::add_member_ptr(const std::string& role, osm::Element* ptr)
{
    members.insert(std::pair<std::string, std::shared_ptr<osm::Element> >(role, std::shared_ptr<osm::Element>(ptr)));
}

}
