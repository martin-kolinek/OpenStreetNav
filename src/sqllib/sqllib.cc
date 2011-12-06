/*
 * sqllib.cc
 *
 *  Created on: Nov 23, 2011
 *      Author: martin
 */

#include "sqllib.h"

namespace sqllib
{

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_create_edges_endnode_fkey(psql::Database& db, bool named, std::string const& name)
{
    std::string str("\
\n\
ALTER TABLE Edges ADD CONSTRAINT FK_Edges_EndNode FOREIGN KEY (EndNodeID) REFERENCES Nodes (ID)\n\
\n\
");
    if (named)
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(name, str, db);
    else
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(str, db);
}

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_create_edges_location_index(psql::Database& db, bool named, std::string const& name)
{
    std::string str("\
\n\
CREATE INDEX IX_EdgesLocation ON Edges USING GIST (Location)\n\
\n\
");
    if (named)
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(name, str, db);
    else
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(str, db);
}

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_create_edges_pkey(psql::Database& db, bool named, std::string const& name)
{
    std::string str("\
\n\
ALTER TABLE Edges ADD CONSTRAINT PK_Edges PRIMARY KEY (WayID, StartNodeID, EndNodeID)\n\
\n\
");
    if (named)
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(name, str, db);
    else
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(str, db);
}

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_create_edges_startnode_fkey(psql::Database& db, bool named, std::string const& name)
{
    std::string str("\
\n\
ALTER TABLE Edges ADD CONSTRAINT FK_Edges_StartNode FOREIGN KEY (StartNodeID) REFERENCES Nodes (ID)\n\
\n\
");
    if (named)
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(name, str, db);
    else
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(str, db);
}

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_create_edges_table(psql::Database& db, bool named, std::string const& name)
{
    std::string str("\
\n\
CREATE TABLE Edges (\n\
WayID bigint,\n\
StartNodeID bigint,\n\
EndNodeID bigint,\n\
Location geography(LINESTRING, 4326)\n\
)\n\
");
    if (named)
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(name, str, db);
    else
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(str, db);
}

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_create_edges_wayid_fkey(psql::Database& db, bool named, std::string const& name)
{
    std::string str("\
\n\
ALTER TABLE Edges ADD CONSTRAINT FK_Edges_Way FOREIGN KEY (WayID) REFERENCES Ways (ID)\n\
\n\
");
    if (named)
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(name, str, db);
    else
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(str, db);
}

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_create_node_attributes(psql::Database& db, bool named, std::string const& name)
{
    std::string str("\
\n\
CREATE TABLE NodeAttributes (\n\
NodeID bigint,\n\
Key text,\n\
Value text\n\
)\n\
\n\
");
    if (named)
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(name, str, db);
    else
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(str, db);
}

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_create_node_members(psql::Database& db, bool named, std::string const& name)
{
    std::string str("\
\n\
CREATE TABLE MemberNodes (\n\
RelationID bigint,\n\
Role text,\n\
NodeID bigint\n\
)\n\
\n\
");
    if (named)
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(name, str, db);
    else
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(str, db);
}

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_create_nodeattr_keyval_index(psql::Database& db, bool named, std::string const& name)
{
    std::string str("\
\n\
CREATE INDEX IX_NodeAttr_KeyVal ON NodeAttributes (Key, Value)\n\
\n\
");
    if (named)
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(name, str, db);
    else
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(str, db);
}

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_create_nodeattributes_nodes_fkey(psql::Database& db, bool named, std::string const& name)
{
    std::string str("\
\n\
ALTER TABLE NodeAttributes ADD CONSTRAINT FK_NodeAttributes_Nodes FOREIGN KEY (NodeID) REFERENCES Nodes (ID)\n\
\n\
");
    if (named)
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(name, str, db);
    else
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(str, db);
}

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_create_nodeattributes_pkey(psql::Database& db, bool named, std::string const& name)
{
    std::string str("\
\n\
ALTER TABLE NodeAttributes ADD CONSTRAINT PK_NodeAttributes PRIMARY KEY (NodeID, Key)\n\
\n\
");
    if (named)
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(name, str, db);
    else
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(str, db);
}

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_create_nodemembers_node_fkey(psql::Database& db, bool named, std::string const& name)
{
    std::string str("\
\n\
ALTER TABLE MemberNodes ADD CONSTRAINT FK_NodeMembers_Node FOREIGN KEY (NodeID) REFERENCES Nodes (ID)\n\
\n\
");
    if (named)
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(name, str, db);
    else
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(str, db);
}

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_create_nodemembers_pkey(psql::Database& db, bool named, std::string const& name)
{
    std::string str("\
\n\
ALTER TABLE MemberNodes ADD CONSTRAINT PK_NodeMembers PRIMARY KEY (RelationID, Role, NodeID)\n\
\n\
");
    if (named)
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(name, str, db);
    else
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(str, db);
}

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_create_nodemembers_relation_fkey(psql::Database& db, bool named, std::string const& name)
{
    std::string str("\
\n\
ALTER TABLE MemberNodes ADD CONSTRAINT FK_NodeMembers_Relation FOREIGN KEY (RelationID) REFERENCES Relations (ID)\n\
\n\
");
    if (named)
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(name, str, db);
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
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(name, str, db);
    else
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(str, db);
}

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_create_nodes_pkey(psql::Database& db, bool named, std::string const& name)
{
    std::string str("\
\n\
ALTER TABLE Nodes ADD CONSTRAINT PK_Nodes PRIMARY KEY (ID)\n\
\n\
");
    if (named)
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(name, str, db);
    else
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(str, db);
}

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_create_nodes_table(psql::Database& db, bool named, std::string const& name)
{
    std::string str("\
\n\
CREATE TABLE Nodes (ID bigint, Location geography(POINT))\n\
\n\
");
    if (named)
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(name, str, db);
    else
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(str, db);
}

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_create_relation_attributes(psql::Database& db, bool named, std::string const& name)
{
    std::string str("\
\n\
CREATE TABLE RelationAttributes (\n\
RelationID bigint,\n\
Key text,\n\
Value text\n\
)\n\
\n\
");
    if (named)
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(name, str, db);
    else
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(str, db);
}

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_create_relationattributes_pkey(psql::Database& db, bool named, std::string const& name)
{
    std::string str("\
\n\
ALTER TABLE RelationAttributes ADD CONSTRAINT PK_RelationAttributes PRIMARY KEY (RelationID, Key)\n\
\n\
");
    if (named)
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(name, str, db);
    else
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(str, db);
}

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_create_relationattributes_relation_fkey(psql::Database& db, bool named, std::string const& name)
{
    std::string str("\
\n\
ALTER TABLE RelationAttributes ADD CONSTRAINT FK_RelationAttributes_Relations FOREIGN KEY (RelationID) REFERENCES Relations (ID)\n\
\n\
");
    if (named)
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(name, str, db);
    else
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(str, db);
}

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_create_relationattributess_pkey(psql::Database& db, bool named, std::string const& name)
{
    std::string str("\
\n\
ALTER TABLE Relations ADD CONSTRAINT PK_Relations PRIMARY KEY (ID)\n\
\n\
");
    if (named)
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(name, str, db);
    else
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(str, db);
}

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_create_relations_pkey(psql::Database& db, bool named, std::string const& name)
{
    std::string str("\
\n\
ALTER TABLE Relations ADD CONSTRAINT PK_Relations PRIMARY KEY (ID)\n\
\n\
");
    if (named)
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(name, str, db);
    else
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(str, db);
}

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_create_relations_table(psql::Database& db, bool named, std::string const& name)
{
    std::string str("\
\n\
CREATE TABLE Relations (\n\
ID bigint\n\
)\n\
");
    if (named)
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(name, str, db);
    else
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(str, db);
}

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_create_relattr_keyval_index(psql::Database& db, bool named, std::string const& name)
{
    std::string str("\
\n\
CREATE INDEX IX_RelAttr_KeyVal ON RelationAttributes (Key, Value)\n\
\n\
");
    if (named)
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(name, str, db);
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
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(name, str, db);
    else
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(str, db);
}

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_create_toshow_table(psql::Database& db, bool named, std::string const& name)
{
    std::string str("\
\n\
CREATE TABLE ToShow (\n\
Key text,\n\
Value text,\n\
Zoom int,\n\
PRIMARY KEY (Key, Value, Zoom)\n\
)\n\
\n\
");
    if (named)
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(name, str, db);
    else
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(str, db);
}

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_create_way_attributes(psql::Database& db, bool named, std::string const& name)
{
    std::string str("\
\n\
CREATE TABLE WayAttributes (\n\
WayID bigint,\n\
Key text,\n\
Value text\n\
)\n\
\n\
");
    if (named)
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(name, str, db);
    else
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(str, db);
}

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_create_way_members_table(psql::Database& db, bool named, std::string const& name)
{
    std::string str("\
\n\
CREATE TABLE MemberWays (\n\
RelationID bigint,\n\
Role text,\n\
WayID bigint\n\
)\n\
\n\
");
    if (named)
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(name, str, db);
    else
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(str, db);
}

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_create_wayattr_keyval_index(psql::Database& db, bool named, std::string const& name)
{
    std::string str("\
\n\
CREATE INDEX IX_WayAttr_KeyVal ON WayAttributes (Key, Value)\n\
\n\
");
    if (named)
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(name, str, db);
    else
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(str, db);
}

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_create_wayattributes_pkey(psql::Database& db, bool named, std::string const& name)
{
    std::string str("\
\n\
ALTER TABLE WayAttributes ADD CONSTRAINT PK_WayAttributes PRIMARY KEY (WayID, Key)\n\
\n\
");
    if (named)
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(name, str, db);
    else
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(str, db);
}

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_create_wayattributes_ways_fkey(psql::Database& db, bool named, std::string const& name)
{
    std::string str("\
\n\
ALTER TABLE WayAttributes ADD CONSTRAINT FK_WayAttributes_Ways FOREIGN KEY (WayID) REFERENCES Ways (ID)\n\
\n\
");
    if (named)
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(name, str, db);
    else
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(str, db);
}

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_create_waymembers_pkey(psql::Database& db, bool named, std::string const& name)
{
    std::string str("\
\n\
ALTER TABLE MemberWays ADD CONSTRAINT PK_WayMembers PRIMARY KEY (RelationID, Role, WayID)\n\
\n\
");
    if (named)
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(name, str, db);
    else
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(str, db);
}

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_create_waymembers_relation_fkey(psql::Database& db, bool named, std::string const& name)
{
    std::string str("\
\n\
ALTER TABLE MemberWays ADD CONSTRAINT FK_WayMembers_Relation FOREIGN KEY (RelationID) REFERENCES Relations (ID)\n\
\n\
");
    if (named)
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(name, str, db);
    else
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(str, db);
}

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_create_waymembers_way_fkey(psql::Database& db, bool named, std::string const& name)
{
    std::string str("\
\n\
ALTER TABLE MemberWays ADD CONSTRAINT FK_WayMembers_Way FOREIGN KEY (WayID) REFERENCES Ways (ID)\n\
\n\
");
    if (named)
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(name, str, db);
    else
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(str, db);
}

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_create_waynodes_node_fkey(psql::Database& db, bool named, std::string const& name)
{
    std::string str("\
\n\
ALTER TABLE WayNodes ADD CONSTRAINT FK_WayNodes_Node FOREIGN KEY (NodeID) REFERENCES Nodes (ID)\n\
\n\
");
    if (named)
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(name, str, db);
    else
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(str, db);
}

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_create_waynodes_pkey(psql::Database& db, bool named, std::string const& name)
{
    std::string str("\
\n\
ALTER TABLE WayNodes ADD CONSTRAINT PK_WayNodes PRIMARY KEY (WayID, SequenceNo)\n\
\n\
");
    if (named)
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(name, str, db);
    else
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(str, db);
}

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_create_waynodes_table(psql::Database& db, bool named, std::string const& name)
{
    std::string str("\
\n\
CREATE TABLE WayNodes (\n\
WayID bigint,\n\
NodeID bigint,\n\
SequenceNo int\n\
)\n\
");
    if (named)
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(name, str, db);
    else
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(str, db);
}

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_create_waynodes_way_fkey(psql::Database& db, bool named, std::string const& name)
{
    std::string str("\
\n\
ALTER TABLE WayNodes ADD CONSTRAINT FK_WayNodes_Way FOREIGN KEY (WayID) REFERENCES Ways (ID)\n\
\n\
");
    if (named)
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(name, str, db);
    else
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(str, db);
}

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_create_ways_pkey(psql::Database& db, bool named, std::string const& name)
{
    std::string str("\
\n\
ALTER TABLE Ways ADD CONSTRAINT PK_Ways PRIMARY KEY (ID)\n\
\n\
");
    if (named)
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(name, str, db);
    else
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(str, db);
}

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_create_ways_table(psql::Database& db, bool named, std::string const& name)
{
    std::string str("\
\n\
CREATE TABLE Ways (ID bigint)\n\
\n\
");
    if (named)
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(name, str, db);
    else
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(str, db);
}

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_drop_edges_endnode_fkey(psql::Database& db, bool named, std::string const& name)
{
    std::string str("\
\n\
ALTER TABLE Edges DROP CONSTRAINT FK_Edges_EndNode\n\
\n\
");
    if (named)
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(name, str, db);
    else
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(str, db);
}

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_drop_edges_location_index(psql::Database& db, bool named, std::string const& name)
{
    std::string str("\
\n\
DROP INDEX IX_EdgesLocation\n\
\n\
");
    if (named)
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(name, str, db);
    else
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(str, db);
}

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_drop_edges_pkey(psql::Database& db, bool named, std::string const& name)
{
    std::string str("\
\n\
ALTER TABLE Edges DROP CONSTRAINT PK_Edges\n\
\n\
");
    if (named)
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(name, str, db);
    else
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(str, db);
}

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_drop_edges_startnode_fkey(psql::Database& db, bool named, std::string const& name)
{
    std::string str("\
\n\
ALTER TABLE Edges DROP CONSTRAINT FK_Edges_StartNode\n\
\n\
");
    if (named)
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(name, str, db);
    else
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(str, db);
}

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_drop_edges_wayid_fkey(psql::Database& db, bool named, std::string const& name)
{
    std::string str("\
\n\
ALTER TABLE Edges DROP CONSTRAINT FK_Edges_Way\n\
\n\
");
    if (named)
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(name, str, db);
    else
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(str, db);
}

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_drop_nodeattr_keyval_index(psql::Database& db, bool named, std::string const& name)
{
    std::string str("\
\n\
DROP INDEX IX_NodeAttr_KeyVal\n\
\n\
");
    if (named)
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(name, str, db);
    else
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(str, db);
}

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_drop_nodeattributes_nodes_fkey(psql::Database& db, bool named, std::string const& name)
{
    std::string str("\
\n\
ALTER TABLE NodeAttributes DROP CONSTRAINT FK_NodeAttributes_Nodes\n\
\n\
");
    if (named)
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(name, str, db);
    else
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(str, db);
}

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_drop_nodeattributes_pkey(psql::Database& db, bool named, std::string const& name)
{
    std::string str("\
\n\
ALTER TABLE NodeAttributes DROP CONSTRAINT PK_NodeAttributes\n\
\n\
");
    if (named)
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(name, str, db);
    else
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(str, db);
}

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_drop_nodemembers_node_fkey(psql::Database& db, bool named, std::string const& name)
{
    std::string str("\
\n\
ALTER TABLE MemberNodes DROP CONSTRAINT FK_NodeMembers_Node\n\
\n\
");
    if (named)
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(name, str, db);
    else
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(str, db);
}

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_drop_nodemembers_pkey(psql::Database& db, bool named, std::string const& name)
{
    std::string str("\
\n\
ALTER TABLE MemberNodes DROP CONSTRAINT PK_NodeMembers\n\
\n\
");
    if (named)
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(name, str, db);
    else
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(str, db);
}

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_drop_nodemembers_relation_fkey(psql::Database& db, bool named, std::string const& name)
{
    std::string str("\
\n\
ALTER TABLE MemberNodes DROP CONSTRAINT FK_NodeMembers_Relation\n\
\n\
");
    if (named)
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(name, str, db);
    else
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(str, db);
}

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_drop_nodes_loc_index(psql::Database& db, bool named, std::string const& name)
{
    std::string str("\
\n\
DROP INDEX IX_NodesLoc\n\
\n\
");
    if (named)
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(name, str, db);
    else
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(str, db);
}

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_drop_nodes_pkey(psql::Database& db, bool named, std::string const& name)
{
    std::string str("\
\n\
ALTER TABLE Nodes DROP CONSTRAINT PK_Nodes\n\
\n\
");
    if (named)
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(name, str, db);
    else
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(str, db);
}

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_drop_relationattributes_pkey(psql::Database& db, bool named, std::string const& name)
{
    std::string str("\
\n\
ALTER TABLE RelationAttributes DROP CONSTRAINT PK_RelationAttributes\n\
\n\
");
    if (named)
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(name, str, db);
    else
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(str, db);
}

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_drop_relationattributes_relation_fkey(psql::Database& db, bool named, std::string const& name)
{
    std::string str("\
\n\
ALTER TABLE RelationAttributes DROP CONSTRAINT FK_RelationAttributes_Relations\n\
\n\
");
    if (named)
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(name, str, db);
    else
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(str, db);
}

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_drop_relations_pkey(psql::Database& db, bool named, std::string const& name)
{
    std::string str("\
\n\
ALTER TABLE Relations DROP CONSTRAINT PK_Relations\n\
\n\
");
    if (named)
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(name, str, db);
    else
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(str, db);
}

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_drop_relattr_keyval_index(psql::Database& db, bool named, std::string const& name)
{
    std::string str("\
\n\
DROP INDEX IX_RelAttr_KeyVal\n\
\n\
");
    if (named)
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(name, str, db);
    else
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(str, db);
}

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_drop_wayattr_keyval_index(psql::Database& db, bool named, std::string const& name)
{
    std::string str("\
\n\
DROP INDEX IX_WayAttr_KeyVal\n\
\n\
");
    if (named)
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(name, str, db);
    else
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(str, db);
}

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_drop_wayattributes_pkey(psql::Database& db, bool named, std::string const& name)
{
    std::string str("\
\n\
ALTER TABLE WayAttributes DROP CONSTRAINT PK_WayAttributes\n\
\n\
");
    if (named)
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(name, str, db);
    else
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(str, db);
}

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_drop_wayattributes_ways_fkey(psql::Database& db, bool named, std::string const& name)
{
    std::string str("\
\n\
ALTER TABLE WayAttributes DROP CONSTRAINT FK_WayAttributes_Ways\n\
\n\
");
    if (named)
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(name, str, db);
    else
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(str, db);
}

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_drop_waymembers_pkey(psql::Database& db, bool named, std::string const& name)
{
    std::string str("\
\n\
ALTER TABLE MemberWays DROP CONSTRAINT PK_WayMembers\n\
\n\
");
    if (named)
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(name, str, db);
    else
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(str, db);
}

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_drop_waymembers_relation_fkey(psql::Database& db, bool named, std::string const& name)
{
    std::string str("\
\n\
ALTER TABLE MemberWays DROP CONSTRAINT FK_WayMembers_Relation\n\
\n\
");
    if (named)
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(name, str, db);
    else
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(str, db);
}

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_drop_waymembers_way_fkey(psql::Database& db, bool named, std::string const& name)
{
    std::string str("\
\n\
ALTER TABLE MemberWays DROP CONSTRAINT FK_WayMembers_Way\n\
\n\
");
    if (named)
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(name, str, db);
    else
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(str, db);
}

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_drop_waynodes_node_fkey(psql::Database& db, bool named, std::string const& name)
{
    std::string str("\
\n\
ALTER TABLE WayNodes DROP CONSTRAINT FK_WayNodes_Node\n\
\n\
");
    if (named)
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(name, str, db);
    else
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(str, db);
}

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_drop_waynodes_pkey(psql::Database& db, bool named, std::string const& name)
{
    std::string str("\
\n\
ALTER TABLE WayNodes DROP CONSTRAINT PK_WayNodes\n\
\n\
");
    if (named)
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(name, str, db);
    else
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(str, db);
}

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_drop_waynodes_way_fkey(psql::Database& db, bool named, std::string const& name)
{
    std::string str("\
\n\
ALTER TABLE WayNodes DROP CONSTRAINT FK_WayNodes_Way\n\
\n\
");
    if (named)
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(name, str, db);
    else
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(str, db);
}

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_drop_ways_pkey(psql::Database& db, bool named, std::string const& name)
{
    std::string str("\
\n\
ALTER TABLE Ways DROP CONSTRAINT PK_Ways\n\
\n\
");
    if (named)
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(name, str, db);
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
        return psql::Statement<psql::BindTypes<int64_t, int64_t, int64_t>, psql::RetTypes<>>(name, str, db);
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
        return psql::Statement<psql::BindTypes<int64_t, double, double>, psql::RetTypes<>>(name, str, db);
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
        return psql::Statement<psql::BindTypes<int64_t, std::string, std::string>, psql::RetTypes<>>(name, str, db);
    else
        return psql::Statement<psql::BindTypes<int64_t, std::string, std::string>, psql::RetTypes<>>(str, db);
}

psql::Statement<psql::BindTypes<int, std::string, int64_t>, psql::RetTypes<>> get_insert_test_table(psql::Database& db, bool named, std::string const& name)
{
    std::string str("\
INSERT INTO TestTable (A, B, C) VALUES ($1, $2, $3)\n\
");
    if (named)
        return psql::Statement<psql::BindTypes<int, std::string, int64_t>, psql::RetTypes<>>(name, str, db);
    else
        return psql::Statement<psql::BindTypes<int, std::string, int64_t>, psql::RetTypes<>>(str, db);
}

psql::Statement<psql::BindTypes<std::string, std::string, int>, psql::RetTypes<>> get_insert_toshow(psql::Database& db, bool named, std::string const& name)
{
    std::string str("\
\n\
INSERT INTO ToShow(Key, Value, Zoom) VALUES ($1, $2, $3)\n\
\n\
");
    if (named)
        return psql::Statement<psql::BindTypes<std::string, std::string, int>, psql::RetTypes<>>(name, str, db);
    else
        return psql::Statement<psql::BindTypes<std::string, std::string, int>, psql::RetTypes<>>(str, db);
}

psql::Statement<psql::BindTypes<int64_t>, psql::RetTypes<>> get_insert_way(psql::Database& db, bool named, std::string const& name)
{
    std::string str("\
\n\
INSERT INTO Ways (ID) VALUES ($1)\n\
\n\
");
    if (named)
        return psql::Statement<psql::BindTypes<int64_t>, psql::RetTypes<>>(name, str, db);
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
        return psql::Statement<psql::BindTypes<int64_t, std::string, std::string>, psql::RetTypes<>>(name, str, db);
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
        return psql::Statement<psql::BindTypes<int64_t, int64_t, int>, psql::RetTypes<>>(name, str, db);
    else
        return psql::Statement<psql::BindTypes<int64_t, int64_t, int>, psql::RetTypes<>>(str, db);
}

psql::Statement<psql::BindTypes<>, psql::RetTypes<double, double, double, double>> get_select_bounds(psql::Database& db, bool named, std::string const& name)
{
    std::string str("\
\n\
select min(ST_X(Location::geometry)), max(ST_X(Location::geometry)), min(ST_Y(Location::geometry)), max(ST_Y(Location::geometry)) FROM Nodes\n\
\n\
");
    if (named)
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<double, double, double, double>>(name, str, db);
    else
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<double, double, double, double>>(str, db);
}

psql::Statement<psql::BindTypes<int, double, double, double, double>, psql::RetTypes<double, double, double, double>> get_select_edges_in_box(psql::Database& db, bool named, std::string const& name)
{
    std::string str("\
\n\
SELECT ST_X(ST_StartPoint(e.Location::geometry)), ST_Y(ST_StartPoint(e.Location::geometry)), ST_X(ST_EndPoint(e.Location::geometry)), ST_Y(ST_EndPoint(e.Location::geometry)) FROM\n\
Edges e INNER JOIN WayAttributes a ON e.WayID = a.WayID INNER JOIN ToShow t ON t.Key = a.Key AND t.Value=a.Value WHERE\n\
t.Zoom=$1 AND e.Location && ST_SetSRID(ST_MakeBox2D(ST_MakePoint($2, $3), ST_MakePoint($4, $5)), -1)\n\
");
    if (named)
        return psql::Statement<psql::BindTypes<int, double, double, double, double>, psql::RetTypes<double, double, double, double>>(name, str, db);
    else
        return psql::Statement<psql::BindTypes<int, double, double, double, double>, psql::RetTypes<double, double, double, double>>(str, db);
}

psql::Statement<psql::BindTypes<int, double, double, double, double>, psql::RetTypes<int64_t, std::string, std::string>> get_select_node_descr_in_box(psql::Database& db, bool named, std::string const& name)
{
    std::string str("\
\n\
SELECT DISTINCT n.ID, a2.Key, a2.Value FROM\n\
NodeAttributes a2 INNER JOIN\n\
Nodes n ON n.ID = a2.NodeID INNER JOIN NodeAttributes a ON n.ID = a.NodeID INNER JOIN ToShow t ON t.Key = a.Key AND t.Value=a.Value WHERE\n\
t.Zoom=$1 AND ST_Intersects(n.Location, ST_SetSRID(ST_MakeBox2D(ST_MakePoint($2, $3), ST_MakePoint($4, $5)), -1)) ORDER BY n.ID\n\
");
    if (named)
        return psql::Statement<psql::BindTypes<int, double, double, double, double>, psql::RetTypes<int64_t, std::string, std::string>>(name, str, db);
    else
        return psql::Statement<psql::BindTypes<int, double, double, double, double>, psql::RetTypes<int64_t, std::string, std::string>>(str, db);
}

psql::Statement<psql::BindTypes<int, double, double, double, double>, psql::RetTypes<double, double>> get_select_nodes_in_box(psql::Database& db, bool named, std::string const& name)
{
    std::string str("\
\n\
SELECT ST_X(n.Location::geometry), ST_Y(n.Location::geometry) FROM\n\
Nodes n INNER JOIN NodeAttributes a ON n.ID = a.NodeID INNER JOIN ToShow t ON t.Key = a.Key AND t.Value=a.Value WHERE\n\
t.Zoom=$1 AND n.Location && ST_SetSRID(ST_MakeBox2D(ST_MakePoint($2, $3), ST_MakePoint($4, $5)), -1)\n\
");
    if (named)
        return psql::Statement<psql::BindTypes<int, double, double, double, double>, psql::RetTypes<double, double>>(name, str, db);
    else
        return psql::Statement<psql::BindTypes<int, double, double, double, double>, psql::RetTypes<double, double>>(str, db);
}

psql::Statement<psql::BindTypes<int, double, double, double, double>, psql::RetTypes<int64_t, std::string, std::string>> get_select_way_descr_in_box(psql::Database& db, bool named, std::string const& name)
{
    std::string str("\
\n\
SELECT DISTINCT e.WayID, a2.Key, a2.Value FROM\n\
WayAttributes a2 INNER JOIN Edges e ON e.WayID = a2.WayID INNER JOIN WayAttributes a ON e.WayID = a.WayID INNER JOIN ToShow t ON t.Key = a.Key AND t.Value=a.Value WHERE\n\
t.Zoom=$1 AND ST_Intersects(e.Location, ST_SetSRID(ST_MakeBox2D(ST_MakePoint($2, $3), ST_MakePoint($4, $5)), -1)) ORDER BY e.WayID\n\
");
    if (named)
        return psql::Statement<psql::BindTypes<int, double, double, double, double>, psql::RetTypes<int64_t, std::string, std::string>>(name, str, db);
    else
        return psql::Statement<psql::BindTypes<int, double, double, double, double>, psql::RetTypes<int64_t, std::string, std::string>>(str, db);
}

psql::Statement<psql::BindTypes<int64_t, int64_t>, psql::RetTypes<int64_t, std::string, std::string>> get_select_wayattributes_between_wayids(psql::Database& db, bool named, std::string const& name)
{
    std::string str("\
\n\
select wayid, key, value from wayattributes where wayid>=$1 and wayid<$2 order by wayid\n\
\n\
");
    if (named)
        return psql::Statement<psql::BindTypes<int64_t, int64_t>, psql::RetTypes<int64_t, std::string, std::string>>(name, str, db);
    else
        return psql::Statement<psql::BindTypes<int64_t, int64_t>, psql::RetTypes<int64_t, std::string, std::string>>(str, db);
}

psql::Statement<psql::BindTypes<int64_t, int64_t>, psql::RetTypes<int64_t, int64_t, double>> get_select_waynodes_for_ways_between(psql::Database& db, bool named, std::string const& name)
{
    std::string str("\
\n\
select w.WayID, w.NodeID, coalesce(ST_Length(e.Location,false), 0) FROM WayNodes w left join Edges e on e.wayid=w.wayid and e.startnodeid = w.nodeid where w.WayID >=$1 and w.WayID<$2 order by w.wayid\n\
");
    if (named)
        return psql::Statement<psql::BindTypes<int64_t, int64_t>, psql::RetTypes<int64_t, int64_t, double>>(name, str, db);
    else
        return psql::Statement<psql::BindTypes<int64_t, int64_t>, psql::RetTypes<int64_t, int64_t, double>>(str, db);
}

psql::Statement<psql::BindTypes<>, psql::RetTypes<int64_t>> get_select_ways(psql::Database& db, bool named, std::string const& name)
{
    std::string str("\
\n\
SELECT ID FROM Ways;\n\
\n\
");
    if (named)
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<int64_t>>(name, str, db);
    else
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<int64_t>>(str, db);
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
        return psql::Statement<psql::BindTypes<int>, psql::RetTypes<int, std::string, int64_t>>(name, str, db);
    else
        return psql::Statement<psql::BindTypes<int>, psql::RetTypes<int, std::string, int64_t>>(str, db);
}


}
