/*
 * OsmDatabase.cpp
 *
 *  Created on: Nov 5, 2011
 *      Author: martin
 */

#include "OsmDatabase.h"

namespace osmdb
{

OsmDatabase::OsmDatabase(std::string const& file):
    nodes_table("Nodes"),
    ways_table("Ways"),
    edges_table("Edges"),
    relations_table("Relations"),
    relation_contents_table("RelationContents"),
    attributes_table("Attributes"),
    db(file),
    nodes_create("CREATE TABLE " + nodes_table + " (ID INTEGER NOT NULL PRIMARY KEY, Latitude REAL NOT NULL, Longitude REAL NOT NULL)"),
    ways_create("CREATE TABLE " + ways_table + " (ID INTEGER NOT NULL PRIMARY KEY)"),
    edges_create("CREATE TABLE " + edges_table + " (ID INTEGER NOT NULL PRIMARY KEY, WayID INTEGER NOT NULL, StartNodeID INTEGER NOT NULL, EndNodeID INTEGER NOT NULL)"),
    relations_create("CREATE TABLE " + relations_table + " (ID INTEGER NOT NULL PRIMARY KEY)"),
    relation_contents_create("CREATE TABLE " + relation_contents_table + " (ID INTEGER NOT NULL PRIMARY KEY, RelationID INTEGER NOT NULL, Role TEXT NOT NULL, ObjectID INTEGER NOT NULL, ObjectType INTEGER NOT NULL)"),
    attributes_create("CREATE TABLE " + attributes_table + " (ID INTEGER NOT NULL PRIMARY KEY, ObjectID INTEGER NOT NULL, ObjectType INTEGER NOT NULL, Key TEXT, Value TEXT)"),
    attr_index1("CREATE INDEX IX_Attributes_Object ON " + attributes_table + "(ObjectID, ObjectType)"),
    attr_index2("CREATE INDEX IX_Attributes_Key ON " + attributes_table + "(Key)"),
    attr_index3("CREATE INDEX IX_Attributes_Value ON " + attributes_table + "(Value)"),
    edge_index_start("CREATE INDEX IX_Edges_Start ON " + edges_table + "(StartNodeID)"),
    edge_index_end("CREATE INDEX IX_Edges_End ON " + edges_table + "(EndNodeID)"),
    edge_index_way("CREATE INDEX IX_Edges_Way ON " + edges_table + "(WayID)"),
    node_index_lat("CREATE INDEX IX_Nodes_Lat ON " + nodes_table + "(Latitude)"),
    node_index_lon("CREATE INDEX IX_Nodes_Lon ON " + nodes_table + "(Longitude)"),
    rel_cont_index_obj("CREATE INDEX IX_RelationContents_Object ON " + relation_contents_table + "(ObjectID, ObjectType)"),
    rel_cont_index_rel("CREATE INDEX IX_RelationContents_Relation ON " + relation_contents_table + "(RelationID)"),
    rel_cont_index_role("CREATE INDEX IX_RelationContents_Relation ON " + relation_contents_table + "(Role)"),
    indexes {attr_index1, attr_index2, attr_index3, edge_index_start, edge_index_end, edge_index_way, node_index_lat, node_index_lon, rel_cont_index_obj, rel_cont_index_rel, rel_cont_index_role},
tables {nodes_create, ways_create, edges_create, relations_create, relation_contents_create, attributes_create}
{
}

OsmDatabase::~OsmDatabase()
{
}

}
