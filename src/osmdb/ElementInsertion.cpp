/*
 * ElementInsertion.cpp
 *
 *  Created on: Nov 6, 2011
 *      Author: martin
 */

#include "ElementInsertion.h"

namespace osmdb
{

ElementInsertion::ElementInsertion(OsmDatabase& db):
		db(db),
		insert_node_sql("INSERT INTO "+db.nodes_table+" (ID, Latitude, Longitude) VALUES (?, ?, ?)"),
		insert_way_sql("INSERT INTO "+db.ways_table+" (ID) VALUES (?)"),
		insert_edge_sql("INSERT INTO "+db.edges_table+" (WayID, StartNodeID, EndNodeID) VALUES (?, ?, ?)"),
		insert_attr_sql("INSERT INTO "+db.attributes_table+" (ObjectID, ObjectType, Key, Value) VALUES (?, ?, ?, ?)"),
		insert_rel_contents_sql("INSERT INTO "+db.relation_contents_table+" (RelationID, Role, ObjectID, ObjectType) VALUES (?, ?, ?, ?)"),
		insert_relation_sql("INSERT INTO "+db.relations_table+" (ID) VALUES (?)"),
		attr_st(insert_attr_sql, db.get_db()),
		node_st(insert_node_sql, db.get_db()),
		edge_st(insert_edge_sql, db.get_db()),
		way_st(insert_way_sql, db.get_db()),
		member_st(insert_rel_contents_sql, db.get_db()),
		rel_st(insert_relation_sql, db.get_db())
{
}

ElementInsertion::~ElementInsertion()
{
}

void ElementInsertion::insert_node(const osm::Node & nd)
{
}

void ElementInsertion::insert_way(const osm::Way & w)
{
}

void ElementInsertion::insert_relation(const osm::Relation & rel)
{
}

}
