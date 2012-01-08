/*
 * ElementCopy.cpp
 *
 *  Created on: Jan 7, 2012
 *      Author: martin
 */

#include "ElementCopy.h"

#include "../sqllib/sqllib.h"

namespace osmdb
{

ElementCopy::ElementCopy(OsmDatabase& db):
    db(db),
    copy(sqllib::get_copy_import(db.get_db()))
{

}

void ElementCopy::start_copy()
{
    copy.execute();
}

void ElementCopy::end_copy()
{
    copy.end_copy();
}

void ElementCopy::insert_node(const osm::Node& nd)
{
    copy.copy_data(1, nd.id, 0, 0, nd.position.lon, nd.position.lat, "", "");
    for (auto it = nd.tags.begin(); it != nd.tags.end(); ++it)
    {
        copy.copy_data(4, nd.id, 0, 0, 0, 0, it->first, it->second);
    }
}

void ElementCopy::insert_way(const osm::Way& w)
{
    copy.copy_data(2, w.id, 0, 0, 0, 0, "", "");
    for (auto it = w.tags.begin(); it != w.tags.end(); ++it)
    {
        copy.copy_data(5, w.id, 0, 0, 0, 0, it->first, it->second);
    }
    for (unsigned int i = 0; i < w.nodes.size(); ++i)
    {
        copy.copy_data(7, w.id, w.nodes[i].id, i, 0, 0, "", "");
    }
}

void ElementCopy::insert_relation(const osm::Relation& rel)
{
    copy.copy_data(3, rel.id, 0, 0, 0, 0, "", "");
    for (auto it = rel.tags.begin(); it != rel.tags.end(); ++it)
    {
        copy.copy_data(6, rel.id, 0, 0, 0, 0, it->first, it->second);
    }
    for (auto it = rel.members.begin(); it != rel.members.end(); ++it)
    {
        it->second->add_to_relation(*this, rel.id, it->first);
    }
}

void ElementCopy::insert_member_node(int64_t rel_id, const std::string& role, int64_t node_id)
{
    copy.copy_data(8, rel_id, node_id, 0, 0, 0, role, "");
}

void ElementCopy::insert_member_way(int64_t rel_id, const std::string& role, int64_t way_id)
{
    copy.copy_data(9, rel_id, way_id, 0, 0, 0, role, "");
}

void ElementCopy::insert_member_relation(int64_t parent_id, const std::string& role, int64_t child_id)
{
    copy.copy_data(10, parent_id, child_id, 0, 0, 0, role, "");
}

ElementCopy::~ElementCopy()
{
}

} /* namespace osmdb */
