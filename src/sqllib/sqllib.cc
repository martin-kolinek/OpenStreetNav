/*
 * sqllib.cc
 *
 *  Created on: Nov 23, 2011
 *      Author: martin
 */

#include "sqllib.h"

namespace sqllib
{

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_create_edges_location_index(psql::Database& db, bool named, std::string const& name)
{
    std::string str("\
\n\
CREATE INDEX IX_EdgesLocation ON Edges USING GIST (Location)\n\
\n\
");
    if (named)
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(str, name, db);
    else
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(str, db);
}

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_create_edges_table(psql::Database& db, bool named, std::string const& name)
{
    std::string str("\
\n\
CREATE TABLE Edges (\n\
WayID bigint REFERENCES Ways (ID),\n\
StartNodeID bigint REFERENCES Nodes (ID),\n\
EndNodeID bigint REFERENCES Nodes (ID),\n\
Location geography(LINESTRING, 4326),\n\
PRIMARY KEY (WayID, StartNodeID, EndNodeID)\n\
)\n\
");
    if (named)
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(str, name, db);
    else
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(str, db);
}

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_create_node_attributes(psql::Database& db, bool named, std::string const& name)
{
    std::string str("\
\n\
CREATE TABLE NodeAttributes (\n\
NodeID bigint REFERENCES Nodes (ID),\n\
Key text,\n\
Value text,\n\
PRIMARY KEY (NodeID, Key)\n\
)\n\
\n\
");
    if (named)
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(str, name, db);
    else
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(str, db);
}

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_create_node_members(psql::Database& db, bool named, std::string const& name)
{
    std::string str("\
\n\
CREATE TABLE MemberNodes (\n\
RelationID bigint REFERENCES Relations (ID),\n\
Role text,\n\
NodeID bigint REFERENCES Nodes (ID),\n\
PRIMARY KEY (RelationID, Role, NodeID)\n\
)\n\
\n\
");
    if (named)
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(str, name, db);
    else
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(str, db);
}

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_create_nodes_loc_index(psql::Database& db, bool named, std::string const& name)
{
    std::string str("\
\n\
CREATE INDEX IX_NodesLoc ON Nodes USING GIST (Location)\n\
\n\
");
    if (named)
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(str, name, db);
    else
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(str, db);
}

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_create_nodes_table(psql::Database& db, bool named, std::string const& name)
{
    std::string str("\
\n\
CREATE TABLE Nodes (ID bigint PRIMARY KEY, Location geography(POINT))\n\
\n\
");
    if (named)
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(str, name, db);
    else
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(str, db);
}

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_create_relation_attributes(psql::Database& db, bool named, std::string const& name)
{
    std::string str("\
\n\
CREATE TABLE NodeAttributes (\n\
RelationID bigint REFERENCES Relations (ID),\n\
Key text,\n\
Value text,\n\
PRIMARY KEY (RelationID, Key)\n\
)\n\
\n\
");
    if (named)
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(str, name, db);
    else
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(str, db);
}

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_create_relations_table(psql::Database& db, bool named, std::string const& name)
{
    std::string str("\
\n\
CREATE TABLE Relations (\n\
ID bigint PRIMARY KEY\n\
)\n\
");
    if (named)
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(str, name, db);
    else
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(str, db);
}

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_create_test_table(psql::Database& db, bool named, std::string const& name)
{
    std::string str("\
\n\
CREATE TABLE TestTable (A int primary key, B text, C bigint)\n\
\n\
");
    if (named)
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(str, name, db);
    else
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(str, db);
}

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_create_way_attributes(psql::Database& db, bool named, std::string const& name)
{
    std::string str("\
\n\
CREATE TABLE WayAttributes (\n\
WayID bigint REFERENCES Ways (ID),\n\
Key text,\n\
Value text,\n\
PRIMARY KEY (WayID, Key)\n\
)\n\
\n\
");
    if (named)
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(str, name, db);
    else
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(str, db);
}

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_create_way_members_table(psql::Database& db, bool named, std::string const& name)
{
    std::string str("\
\n\
CREATE TABLE MemberWays (\n\
RelationID bigint REFERENCES Relations (ID),\n\
Role text,\n\
WayID bigint REFERENCES Ways (ID),\n\
PRIMARY KEY (RelationID, Role, WayID)\n\
)\n\
\n\
");
    if (named)
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(str, name, db);
    else
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(str, db);
}

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_create_waynodes_table(psql::Database& db, bool named, std::string const& name)
{
    std::string str("\
\n\
CREATE TABLE WayNodes (\n\
WayID bigint REFERENCES Ways (ID),\n\
NodeID bigint REFERENCES Nodes (ID),\n\
SequenceNo int,\n\
PRIMARY KEY (WayID, SequenceNo)\n\
)\n\
");
    if (named)
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(str, name, db);
    else
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(str, db);
}

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_create_ways_table(psql::Database& db, bool named, std::string const& name)
{
    std::string str("\
\n\
CREATE TABLE Ways (ID bigint PRIMARY KEY)\n\
\n\
");
    if (named)
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(str, name, db);
    else
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(str, db);
}

psql::Statement<psql::BindTypes<int64_t, int64_t, int64_t>, psql::RetTypes<>> get_insert_edge(psql::Database& db, bool named, std::string const& name)
{
    std::string str("\
\n\
INSERT INTO Edges (WayID, StartNodeID, EndNodeID, Location)\n\
SELECT $1, $2, $3, ST_MakeLine(n1.Location::geometry, n2.Location::geometry)::geography FROM Nodes n1, Nodes n2 WHERE n1.ID=$2 AND n2.ID=$3\n\
\n\
");
    if (named)
        return psql::Statement<psql::BindTypes<int64_t, int64_t, int64_t>, psql::RetTypes<>>(str, name, db);
    else
        return psql::Statement<psql::BindTypes<int64_t, int64_t, int64_t>, psql::RetTypes<>>(str, db);
}

psql::Statement<psql::BindTypes<int64_t, double, double>, psql::RetTypes<>> get_insert_node(psql::Database& db, bool named, std::string const& name)
{
    std::string str("\
\n\
INSERT INTO Nodes (ID, Location) VALUES ($1, ST_MakePoint($2, $3))\n\
\n\
");
    if (named)
        return psql::Statement<psql::BindTypes<int64_t, double, double>, psql::RetTypes<>>(str, name, db);
    else
        return psql::Statement<psql::BindTypes<int64_t, double, double>, psql::RetTypes<>>(str, db);
}

psql::Statement<psql::BindTypes<int64_t, std::string, std::string>, psql::RetTypes<>> get_insert_node_attr(psql::Database& db, bool named, std::string const& name)
{
    std::string str("\
\n\
INSERT INTO NodeAttributes (NodeID, Key, Value) VALUES ($1, $2, $3)\n\
\n\
");
    if (named)
        return psql::Statement<psql::BindTypes<int64_t, std::string, std::string>, psql::RetTypes<>>(str, name, db);
    else
        return psql::Statement<psql::BindTypes<int64_t, std::string, std::string>, psql::RetTypes<>>(str, db);
}

psql::Statement<psql::BindTypes<int, std::string, int64_t>, psql::RetTypes<>> get_insert_test_table(psql::Database& db, bool named, std::string const& name)
{
    std::string str("\
INSERT INTO TestTable (A, B, C) VALUES ($1, $2, $3)\n\
");
    if (named)
        return psql::Statement<psql::BindTypes<int, std::string, int64_t>, psql::RetTypes<>>(str, name, db);
    else
        return psql::Statement<psql::BindTypes<int, std::string, int64_t>, psql::RetTypes<>>(str, db);
}

psql::Statement<psql::BindTypes<int64_t>, psql::RetTypes<>> get_insert_way(psql::Database& db, bool named, std::string const& name)
{
    std::string str("\
\n\
INSERT INTO Ways (ID) VALUES ($1)\n\
\n\
");
    if (named)
        return psql::Statement<psql::BindTypes<int64_t>, psql::RetTypes<>>(str, name, db);
    else
        return psql::Statement<psql::BindTypes<int64_t>, psql::RetTypes<>>(str, db);
}

psql::Statement<psql::BindTypes<int64_t, std::string, std::string>, psql::RetTypes<>> get_insert_way_attr(psql::Database& db, bool named, std::string const& name)
{
    std::string str("\
\n\
INSERT INTO WayAttributes (WayID, Key, Value) VALUES ($1, $2, $3)\n\
\n\
");
    if (named)
        return psql::Statement<psql::BindTypes<int64_t, std::string, std::string>, psql::RetTypes<>>(str, name, db);
    else
        return psql::Statement<psql::BindTypes<int64_t, std::string, std::string>, psql::RetTypes<>>(str, db);
}

psql::Statement<psql::BindTypes<int64_t, int64_t, int>, psql::RetTypes<>> get_insert_way_node(psql::Database& db, bool named, std::string const& name)
{
    std::string str("\
\n\
INSERT INTO WayNodes (WayID, NodeID, SequenceNo) VALUES ($1, $2, $3)\n\
\n\
");
    if (named)
        return psql::Statement<psql::BindTypes<int64_t, int64_t, int>, psql::RetTypes<>>(str, name, db);
    else
        return psql::Statement<psql::BindTypes<int64_t, int64_t, int>, psql::RetTypes<>>(str, db);
}

psql::Statement<psql::BindTypes<int>, psql::RetTypes<int, std::string, int64_t>> get_test_select(psql::Database& db, bool named, std::string const& name)
{
    std::string str("\
\n\
SELECT * FROM TestTable\n\
WHERE\n\
--comment\n\
A = $1;\n\
\n\
");
    if (named)
        return psql::Statement<psql::BindTypes<int>, psql::RetTypes<int, std::string, int64_t>>(str, name, db);
    else
        return psql::Statement<psql::BindTypes<int>, psql::RetTypes<int, std::string, int64_t>>(str, db);
}


}
