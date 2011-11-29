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
    way_attrs_ins(sqllib::get_insert_way_attr(db.get_db(), true, "way_attrs_ins_st"))
{

}

osmdb::ElementInsertion::~ElementInsertion()
{
}

void osmdb::ElementInsertion::insert_node(const osm::Node& nd)
{
    db.get_db().savepoint("node_ins_savepoint");
    try
    {
        node_ins.execute(nd.id, nd.lon, nd.lat);
        for (auto it = nd.tags.begin(); it != nd.tags.end(); ++it)
        {
            node_attrs_ins.execute(nd.id, it->key, it->value);
        }
    }
    catch (psql::PgSqlException& ex)
    {
        db.get_db().rollback_to_savepoint("node_ins_savepoint");
    }
}

void osmdb::ElementInsertion::insert_way(const osm::Way& w)
{
    db.get_db().savepoint("way_ins_savepoint");
    try
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
    catch (psql::PgSqlException& ex)
    {
        db.get_db().rollback_to_savepoint("way_ins_savepoint");
    }
}

void osmdb::ElementInsertion::insert_relation(const osm::Relation&)
{
    //TODO
}

}

