/*
 * OsmDatabase.cpp
 *
 *  Created on: Nov 5, 2011
 *      Author: martin
 */

#include "OsmDatabase.h"
#include "WrongDBException.h"

namespace osmdb
{

OsmDatabase::OsmDatabase(const std::string& file)
    : nodes_table("Nodes"),
      ways_table("Ways"),
      edges_table("Edges"),
      relations_table("Relations"),
      relation_contents_table("RelationContents"),
      attributes_table("Attributes"),
      db(file),
      nodes_create(get_nodes_create(nodes_table)),
      ways_create(get_ways_create(ways_table)),
      edges_create(get_edges_create(edges_table)),
      relations_create(get_relations_create(relations_table)),
      relation_contents_create(get_relation_contents_create(relation_contents_table)),
      attributes_create(get_attributes_create(attributes_table)),
      attr_index1(get_attr_index1(attributes_table)),
      attr_index2(get_attr_index2(attributes_table)),
      attr_index3(get_attr_index3(attributes_table)),
      edge_index_start(get_edge_index_start(edges_table)),
      edge_index_end(get_edge_index_end(edges_table)),
      edge_index_way(get_edge_index_way(edges_table)),
      node_index_lat(get_node_index_lat(nodes_table)),
      node_index_lon(get_node_index_lon(nodes_table)),
      rel_cont_index_obj(get_rel_cont_index_obj(relation_contents_table)),
      rel_cont_index_rel(get_rel_cont_index_rel(relation_contents_table)),
      rel_cont_index_role(get_rel_cont_index_role(relation_contents_table)),
      nodes_test("SELECT ID, Latitude, Longitude FROM " + nodes_table + " LIMIT 1"),
      ways_test("SELECT ID FROM " + ways_table + " LIMIT 1"),
      edge_test("SELECT WayID, StartNodeID, EndNodeID FROM " + edges_table + " LIMIT 1"),
      relation_test("SELECT ID FROM " + relations_table + " LIMIT 1"),
      rel_contents_test("SELECT RelationID, Role, ObjectID, ObjectType FROM " + relation_contents_table + " LIMIT 1"),
      attributes_test("SELECT ObjectID, ObjectType, Key, Value FROM " + attributes_table + " LIMIT 1"),
      indexes {attr_index1, attr_index2, attr_index3, edge_index_start, edge_index_end, edge_index_way, node_index_lat, node_index_lon, rel_cont_index_obj, rel_cont_index_rel, rel_cont_index_role},
          tables {nodes_create, ways_create, edges_create, relations_create, relation_contents_create, attributes_create},
checks {nodes_test, ways_test, edge_test, relation_test, rel_contents_test, attributes_test}
{
    if (db.is_new())
        create_tables();

    try {
        check_tables();
    }
    catch (sqlite::SqliteException& ex)
    {
        throw WrongDBException(file);
    }
}

OsmDatabase::~OsmDatabase()
{
}

sqlite::Database& OsmDatabase::get_db()
{
    return db;
}

void OsmDatabase::create_indexes()
{
    for (auto it = indexes.begin(); it != indexes.end(); ++it)
    {
        sqlite::execute_sql(*it, db);
    }
}

void OsmDatabase::create_tables()
{
    for (auto it = tables.begin(); it != tables.end(); ++it)
    {
        sqlite::execute_sql(*it, db);
    }
}

std::string OsmDatabase::get_nodes_create(const std::string& nodes_table)
{
    return "CREATE TABLE " + nodes_table + " (ID INTEGER NOT NULL PRIMARY KEY, Latitude REAL NOT NULL, Longitude REAL NOT NULL)";
}

std::string OsmDatabase::get_ways_create(const std::string& ways_table)
{
    return "CREATE TABLE " + ways_table + " (ID INTEGER NOT NULL PRIMARY KEY)";
}

std::string OsmDatabase::get_edges_create(const std::string& edges_table)
{
    return "CREATE TABLE " + edges_table + " (ID INTEGER NOT NULL PRIMARY KEY, WayID INTEGER NOT NULL, StartNodeID INTEGER NOT NULL, EndNodeID INTEGER NOT NULL)";
}

std::string OsmDatabase::get_relations_create(const std::string& relations_table)
{
    return "CREATE TABLE " + relations_table + " (ID INTEGER NOT NULL PRIMARY KEY)";
}

std::string OsmDatabase::get_relation_contents_create(const std::string& relation_contents_table)
{
    return "CREATE TABLE " + relation_contents_table + " (ID INTEGER NOT NULL PRIMARY KEY, RelationID INTEGER NOT NULL, Role TEXT NOT NULL, ObjectID INTEGER NOT NULL, ObjectType INTEGER NOT NULL)";
}

std::string OsmDatabase::get_attributes_create(const std::string& attributes_table)
{
    return "CREATE TABLE " + attributes_table + " (ID INTEGER NOT NULL PRIMARY KEY, ObjectID INTEGER NOT NULL, ObjectType INTEGER NOT NULL, Key TEXT, Value TEXT)";
}

std::string OsmDatabase::get_attr_index1(const std::string& attributes_table)
{
    return "CREATE INDEX IX_Attributes_Object ON " + attributes_table + "(ObjectID, ObjectType)";

}

std::string OsmDatabase::get_attr_index2(const std::string& attributes_table)
{
    return "CREATE INDEX IX_Attributes_Key ON " + attributes_table + "(Key)";

}

std::string OsmDatabase::get_attr_index3(const std::string& attributes_table)
{
    return "CREATE INDEX IX_Attributes_Value ON " + attributes_table + "(Value)";

}

std::string OsmDatabase::get_edge_index_start(const std::string& edges_table)
{
    return "CREATE INDEX IX_Edges_Start ON " + edges_table + "(StartNodeID)";

}

std::string OsmDatabase::get_edge_index_end(const std::string& edges_table)
{
    return "CREATE INDEX IX_Edges_End ON " + edges_table + "(EndNodeID)";

}

std::string OsmDatabase::get_edge_index_way(const std::string& edges_table)
{
    return "CREATE INDEX IX_Edges_Way ON " + edges_table + "(WayID)";

}

std::string OsmDatabase::get_node_index_lat(const std::string& nodes_table)
{
    return "CREATE INDEX IX_Nodes_Lat ON " + nodes_table + "(Latitude)";

}

std::string OsmDatabase::get_node_index_lon(const std::string& nodes_table)
{
    return "CREATE INDEX IX_Nodes_Lon ON " + nodes_table + "(Longitude)";

}

std::string OsmDatabase::get_rel_cont_index_obj(const std::string& relation_contents_table)
{
    return "CREATE INDEX IX_RelationContents_Object ON " + relation_contents_table + "(ObjectID, ObjectType)";

}

std::string OsmDatabase::get_rel_cont_index_rel(const std::string& relation_contents_table)
{
    return "CREATE INDEX IX_RelationContents_Relation ON " + relation_contents_table + "(RelationID)";

}

std::string OsmDatabase::get_rel_cont_index_role(const std::string& relation_contents_table)
{
    return "CREATE INDEX IX_RelationContents_Role ON " + relation_contents_table + "(Role)";
}

void OsmDatabase::check_tables()
{
    for (auto it = checks.begin(); it != checks.end(); ++it)
    {
        sqlite::execute_sql(*it, db);
    }
}

}
