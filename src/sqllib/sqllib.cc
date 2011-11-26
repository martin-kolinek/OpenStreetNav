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
PRIMARY KEY (RelationID, Role)\n\
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
CREATE TABLE NodeAttributes (\n\
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
PRIMARY KEY (RelationID, Role)\n\
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
