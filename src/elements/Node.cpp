/*
 * Node.cpp
 *
 *  Created on: Nov 5, 2011
 *      Author: martin
 */

#include "Node.h"
#include "../util.h"
#include "../osmdb/osmdb.h"

namespace osm
{

Node::Node()
{
}

Node::Node(int64_t id, double lat, double lon):
    id(id),
    position(lat, lon)
{
}

Node::~Node()
{
}

bool Node::operator==(const Node& other) const
{
    return id == other.id && position == other.position && tags == other.tags;
}

boost::property_tree::ptree Node::get_description()
{
    boost::property_tree::ptree ret;
    boost::property_tree::ptree node;
    node.data() = util::to_str(id);
    boost::property_tree::ptree tags_desc;
    for (auto it = tags.begin(); it != tags.end(); ++it)
    {
        tags_desc.push_back(*it);
    }
    node.put_child("tags", tags_desc);
    ret.put_child("node", node);
    return ret;
}

void Node::fill(osmdb::PropertiesSelection& db)
{
    auto p = db.get_position(id);
    position = p;
    tags = db.get_node_tags(id);
}

void Node::add_to_relation(osmdb::ElementImporter& db, int64_t relation, const std::string& role)
{
    db.insert_member_node(relation, role, id);
}

osm::ObjectType Node::get_type() const
{
    return osm::ObjectType::Node;
}

bool Node::operator !=(const Node& other) const
{
    return !(*this == other);
}

int64_t Node::get_id() const
{
    return id;
}

}
