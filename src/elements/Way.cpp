/*
 * Way.cpp
 *
 *  Created on: Nov 5, 2011
 *      Author: martin
 */

#include "Way.h"
#include "../osmdb/osmdb.h"

namespace osm
{

Way::Way()
{
}

Way::Way(int64_t id):
    id(id)
{
}

Way::~Way()
{
}

bool Way::operator==(const Way& other) const
{
    return id == other.id && nodes == other.nodes && tags == other.tags;
}

boost::property_tree::ptree Way::get_description() const
{
    boost::property_tree::ptree ret;
    boost::property_tree::ptree way;
    way.data() = util::to_str(id);
    boost::property_tree::ptree tags_desc;
    for (auto it = tags.begin(); it != tags.end(); ++it)
    {
        tags_desc.push_back(*it);
    }
    way.put_child("tags", tags_desc);
    boost::property_tree::ptree nds;
    for (auto it = nodes.begin(); it != nodes.end(); ++it)
    {
        nds.push_back(it->second.get_description().front());
    }
    way.put_child("nodes", nds);
    ret.put_child("way", way);
    return ret;
}

void Way::fill(osmdb::PropertiesSelection& db)
{
    tags = db.get_way_tags(id);
    auto m =  db.get_waynodes(id);
    for (auto it = m.begin(); it != m.end(); ++it)
    {
        nodes.insert(std::make_pair(it->first, Node(it->second)));
    }

    for (unsigned int i = 0; i < nodes.size(); ++i)
    {
        nodes[i].fill(db);
    }
}

std::vector<std::unique_ptr<osm::WayRegion> > Way::get_regions() const
{
    std::vector<std::unique_ptr<osm::WayRegion> > ret;
    ret.push_back(std::unique_ptr<osm::WayRegion>(new osm::WayRegion(*this)));
    return ret;
}

int64_t Way::get_id() const
{
    return id;
}

void Way::add_to_relation(osmdb::ElementImporter& db, int64_t relation, const std::string& role)
{
    db.insert_member_way(relation, role, id);
}

osm::ObjectType Way::get_type() const
{
    return osm::ObjectType::Way;
}

bool Way::operator !=(const Way& other) const
{
    return !(*this == other);
}

void Way::add_node(osm::Node const& nd, int seq)
{
    if (seq < 0)
    {
        auto it = nodes.end();
        if (it == nodes.begin())
            seq = 0;
        else
            seq = (--it)->first + 1;

    }

    nodes.insert(std::make_pair(seq, nd));

}

}
