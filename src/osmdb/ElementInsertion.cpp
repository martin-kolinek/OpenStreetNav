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
    node_ins(sqllib::get_insert_node(db.get_db())),
    edge_ins(sqllib::get_insert_edge(db.get_db())),
    waynode_ins(sqllib::get_insert_way_node(db.get_db())),
    way_ins(sqllib::get_insert_way(db.get_db())),
    node_attrs_ins(sqllib::get_insert_node_attr(db.get_db())),
    way_attrs_ins(sqllib::get_insert_way_attr(db.get_db()))
{

}

osmdb::ElementInsertion::~ElementInsertion()
{
}

void osmdb::ElementInsertion::insert_node(const osm::Node& nd)
{
    node_ins.execute(nd.id, nd.lon, nd.lat);
    for (auto it = nd.tags.begin(); it != nd.tags.end(); ++it)
    {
        node_attrs_ins.execute(nd.id, it->key, it->value);
    }
}

void osmdb::ElementInsertion::insert_way(const osm::Way& w)
{
    way_ins.execute(w.id);
    int i = 0;
    for (auto it = w.nodes.begin(); it != w.nodes.end(); ++it)
    {
        waynode_ins.execute(w.id, *it, i++);
        auto it2 = it + 1;
        if (it2 == w.nodes.end())
            break;
        edge_ins.execute(w.id, *it, *it2);
    }
    for (auto it = w.tags.begin(); it != w.tags.end(); ++it)
    {
        way_attrs_ins.execute(w.id, it->key, it->value);
    }
}

void osmdb::ElementInsertion::insert_relation(const osm::Relation&)
{
    //TODO
}

}

