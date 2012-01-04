/*
 * ElementInsertion.cpp
 *
 *  Created on: Nov 6, 2011
 *      Author: martin
 */

#include "ElementInsertion.h"
#include "../sqllib/sqllib.h"

namespace osmdb
{

osmdb::ElementInsertion::ElementInsertion(OsmDatabase& db):
    db(db),
    node_ins(sqllib::get_insert_node(db.get_db(), true, "node_ins_st")),
    edge_ins(sqllib::get_insert_edge(db.get_db(), true, "edge_ins_st")),
    waynode_ins(sqllib::get_insert_way_node(db.get_db(), true, "waynode_ins_st")),
    way_ins(sqllib::get_insert_way(db.get_db(), true, "way_ins_st")),
    node_attrs_ins(sqllib::get_insert_node_attr(db.get_db(), true, "node_attrs_ins_st")),
    way_attrs_ins(sqllib::get_insert_way_attr(db.get_db(), true, "way_attrs_ins_st")),
    rel_attrs_ins(sqllib::get_insert_rel_attribute(db.get_db(), true, "rel_attrs_ins_st")),
    rel_ins(sqllib::get_insert_relation(db.get_db(), true, "rel_ins_st")),
    node_mem_ins(sqllib::get_insert_node_member(db.get_db(), true, "node_member_ins_st")),
    way_mem_ins(sqllib::get_insert_way_member(db.get_db(), true, "way_member_ins_st")),
    rel_mem_ins(sqllib::get_insert_relation_member(db.get_db(), true, "rel_member_ins_st"))
{

}

osmdb::ElementInsertion::~ElementInsertion()
{
}

void osmdb::ElementInsertion::insert_node(const osm::Node& nd)
{
    node_ins.execute(nd.id, nd.position.lon, nd.position.lat);
    for (auto it = nd.tags.begin(); it != nd.tags.end(); ++it)
    {
        node_attrs_ins.execute(nd.id, it->first, it->second);
    }
}

void osmdb::ElementInsertion::insert_way(const osm::Way& w)
{
    way_ins.execute(w.id);
    int i = 0;
    for (auto it = w.nodes.begin(); it != w.nodes.end(); ++it)
    {
        waynode_ins.execute(w.id, it->id, i++);
        auto it2 = it + 1;
        if (it2 == w.nodes.end())
            break;
        edge_ins.execute(w.id, it->id, it2->id);
    }
    for (auto it = w.tags.begin(); it != w.tags.end(); ++it)
    {
        way_attrs_ins.execute(w.id, it->first, it->second);
    }
}

void osmdb::ElementInsertion::insert_relation(const osm::Relation& r)
{
    rel_ins.execute(r.id);
    for (auto it = r.tags.begin(); it != r.tags.end(); ++it)
    {
        rel_attrs_ins.execute(r.id, it->first, it->second);
    }
    for (auto it = r.members.begin(); it != r.members.end(); ++it)
    {
        it->second->add_to_relation(*this, r.id, it->first);
    }
}

void ElementInsertion::insert_member_node(int64_t rel_id, const std::string& role, int64_t node_id)
{
    node_mem_ins.execute(rel_id, role, node_id);
}

void ElementInsertion::insert_member_way(int64_t rel_id, const std::string& role, int64_t way_id)
{
    way_mem_ins.execute(rel_id, role, way_id);
}

void ElementInsertion::insert_member_relation(int64_t parent_id, const std::string& role, int64_t child_id)
{
    rel_mem_ins.execute(parent_id, role, child_id);
}

}

