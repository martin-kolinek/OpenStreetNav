--name create_import_seq
--type psql::BindTypes<>, psql::RetTypes<>
--test-param

CREATE SEQUENCE Import_Seq

--name create_import_table
--type psql::BindTypes<>, psql::RetTypes<>
--test-depend create_import_seq
--test-param

CREATE TABLE Import (
	ID bigint DEFAULT NEXTVAL('Import_Seq'),
	EntryType int null,
	biginta bigint null,
	bigintb bigint null,
	inta int null,
	doublea float8 null,
	doubleb float8 null,
	stringa text null,
	stringb text null
)

--name copy_import
--type psql::BindTypes<>, psql::RetTypes<>, psql::CopyTypes<int, int64_t, int64_t, int, double, double, std::string, std::string>
--test-depend create_import_seq
--test-depend create_import_table
--test-param

COPY Import (EntryType, biginta, bigintb, inta, doublea, doubleb, stringa, stringb) FROM STDIN

--name create_import_pkey
--type psql::BindTypes<>, psql::RetTypes<>
--test-depend create_import_seq
--test-depend create_import_table
--test-param

ALTER TABLE Import ADD CONSTRAINT PK_Import PRIMARY KEY (ID)

--name drop_import_pkey
--type psql::BindTypes<>, psql::RetTypes<>
--test-depend create_import_seq
--test-depend create_import_table
--test-depend create_import_pkey
--test-param

ALTER TABLE Import DROP CONSTRAINT PK_Import

--name create_import_index
--type psql::BindTypes<>, psql::RetTypes<>
--test-depend create_import_seq
--test-depend create_import_table
--test-param

CREATE INDEX IX_Import_TypeBigintaID ON Import (EntryType, biginta, ID)

--name drop_import_index
--type psql::BindTypes<>, psql::RetTypes<>
--test-depend create_import_seq
--test-depend create_import_table
--test-depend create_import_index
--test-param

DROP INDEX IX_Import_TypeBigintaID

--name create_import_index_edge
--type psql::BindTypes<>, psql::RetTypes<>
--test-depend create_import_seq
--test-depend create_import_table
--test-param

CREATE INDEX IX_Import_Edge ON Import(EntryType, biginta, (inta + 1))

--name drop_import_index_edge
--type psql::BindTypes<>, psql::RetTypes<>
--test-depend create_import_seq
--test-depend create_import_table
--test-depend create_import_index_edge
--test-param

DROP INDEX IX_Import_Edge

--name analyze_import
--type psql::BindTypes<>, psql::RetTypes<>
--test-depend create_import_seq
--test-depend create_import_table
--test-param

ANALYZE Import

--name delete_updated_nodes
--type psql::BindTypes<>, psql::RetTypes<>
--test-depend create_import_seq
--test-depend create_import_table
--test-depend create_nodes_table
--test-depend create_ways_table
--test-depend create_relations_table
--test-depend create_node_attributes
--test-depend create_node_members
--test-depend create_waynodes_table
--test-depend create_way_attributes
--test-depend create_way_members_table
--test-depend create_relation_members
--test-depend create_relation_attributes
--test-param

DELETE FROM Nodes WHERE EXISTS (SELECT i.biginta FROM Import i where i.EntryType=1 AND Nodes.ID = i.biginta)

--name delete_updated_ways
--type psql::BindTypes<>, psql::RetTypes<>
--test-depend create_import_seq
--test-depend create_import_table
--test-depend create_nodes_table
--test-depend create_ways_table
--test-depend create_relations_table
--test-depend create_node_attributes
--test-depend create_node_members
--test-depend create_waynodes_table
--test-depend create_way_attributes
--test-depend create_way_members_table
--test-depend create_relation_members
--test-depend create_relation_attributes
--test-param

DELETE FROM Ways WHERE EXISTS (SELECT i.biginta FROM Import i where i.EntryType=2 AND Ways.ID = i.biginta)

--name delete_updated_relations
--type psql::BindTypes<>, psql::RetTypes<>
--test-depend create_import_seq
--test-depend create_import_table
--test-depend create_nodes_table
--test-depend create_ways_table
--test-depend create_relations_table
--test-depend create_node_attributes
--test-depend create_node_members
--test-depend create_waynodes_table
--test-depend create_way_attributes
--test-depend create_way_members_table
--test-depend create_relation_members
--test-depend create_relation_attributes
--test-param

DELETE FROM Relations WHERE EXISTS (SELECT i.biginta FROM Import i where i.EntryType=3 AND Relations.ID = i.biginta)

--name delete_deleted_nodes
--type psql::BindTypes<>, psql::RetTypes<>
--test-depend create_import_seq
--test-depend create_import_table
--test-depend create_nodes_table
--test-depend create_ways_table
--test-depend create_relations_table
--test-depend create_node_attributes
--test-depend create_node_members
--test-depend create_waynodes_table
--test-depend create_way_attributes
--test-depend create_way_members_table
--test-depend create_relation_members
--test-depend create_relation_attributes
--test-param

DELETE FROM Nodes WHERE EXISTS (SELECT i.biginta FROM Import i where i.EntryType=11 AND Nodes.ID = i.biginta)

--name delete_deleted_ways
--type psql::BindTypes<>, psql::RetTypes<>
--test-depend create_import_seq
--test-depend create_import_table
--test-depend create_nodes_table
--test-depend create_ways_table
--test-depend create_relations_table
--test-depend create_node_attributes
--test-depend create_node_members
--test-depend create_waynodes_table
--test-depend create_way_attributes
--test-depend create_way_members_table
--test-depend create_relation_members
--test-depend create_relation_attributes
--test-param

DELETE FROM Ways WHERE EXISTS (SELECT i.biginta FROM Import i where i.EntryType=12 AND Ways.ID = i.biginta)

--name delete_deleted_relations
--type psql::BindTypes<>, psql::RetTypes<>
--test-depend create_import_seq
--test-depend create_import_table
--test-depend create_nodes_table
--test-depend create_ways_table
--test-depend create_relations_table
--test-depend create_node_attributes
--test-depend create_node_members
--test-depend create_waynodes_table
--test-depend create_way_attributes
--test-depend create_way_members_table
--test-depend create_relation_members
--test-depend create_relation_attributes
--test-param

DELETE FROM Relations WHERE EXISTS (SELECT i.biginta FROM Import i where i.EntryType=13 AND Relations.ID = i.biginta)

--name delete_orphan1
--type psql::BindTypes<>, psql::RetTypes<>
--test-depend create_import_seq
--test-depend create_import_table
--test-depend create_nodes_table
--test-depend create_ways_table
--test-depend create_relations_table
--test-depend create_node_attributes
--test-depend create_node_members
--test-depend create_waynodes_table
--test-depend create_way_attributes
--test-depend create_way_members_table
--test-depend create_relation_members
--test-depend create_relation_attributes
--test-param

DELETE FROM NodeAttributes WHERE NOT EXISTS(SELECT n.ID FROM Nodes n WHERE n.ID = NodeAttributes.NodeID)

--name delete_orphan2
--type psql::BindTypes<>, psql::RetTypes<>
--test-depend create_import_seq
--test-depend create_import_table
--test-depend create_nodes_table
--test-depend create_ways_table
--test-depend create_relations_table
--test-depend create_node_attributes
--test-depend create_node_members
--test-depend create_waynodes_table
--test-depend create_way_attributes
--test-depend create_way_members_table
--test-depend create_relation_members
--test-depend create_relation_attributes
--test-param

DELETE FROM WayAttributes WHERE NOT EXISTS(SELECT w.ID FROM Ways w WHERE w.ID = WayAttributes.WayID)

--name delete_orphan3
--type psql::BindTypes<>, psql::RetTypes<>
--test-depend create_import_seq
--test-depend create_import_table
--test-depend create_nodes_table
--test-depend create_ways_table
--test-depend create_relations_table
--test-depend create_node_attributes
--test-depend create_node_members
--test-depend create_waynodes_table
--test-depend create_way_attributes
--test-depend create_way_members_table
--test-depend create_relation_members
--test-depend create_relation_attributes
--test-param

DELETE FROM RelationAttributes WHERE NOT EXISTS(SELECT r.ID FROM Relations r WHERE r.ID = RelationAttributes.RelationID)

--name delete_orphan4
--type psql::BindTypes<>, psql::RetTypes<>
--test-depend create_import_seq
--test-depend create_import_table
--test-depend create_nodes_table
--test-depend create_ways_table
--test-depend create_relations_table
--test-depend create_node_attributes
--test-depend create_node_members
--test-depend create_waynodes_table
--test-depend create_way_attributes
--test-depend create_way_members_table
--test-depend create_relation_members
--test-depend create_relation_attributes
--test-param

DELETE FROM WayNodes WHERE NOT EXISTS(SELECT w.ID FROM Ways w WHERE w.ID=WayNodes.WayID)

--name delete_orphan5
--type psql::BindTypes<>, psql::RetTypes<>
--test-depend create_import_seq
--test-depend create_import_table
--test-depend create_nodes_table
--test-depend create_ways_table
--test-depend create_relations_table
--test-depend create_node_attributes
--test-depend create_node_members
--test-depend create_waynodes_table
--test-depend create_way_attributes
--test-depend create_way_members_table
--test-depend create_relation_members
--test-depend create_relation_attributes
--test-param

DELETE FROM MemberNodes WHERE NOT EXISTS(SELECT r.ID FROM Relations r WHERE r.ID=MemberNodes.RelationID)

--name delete_orphan6
--type psql::BindTypes<>, psql::RetTypes<>
--test-depend create_import_seq
--test-depend create_import_table
--test-depend create_nodes_table
--test-depend create_ways_table
--test-depend create_relations_table
--test-depend create_node_attributes
--test-depend create_node_members
--test-depend create_waynodes_table
--test-depend create_way_attributes
--test-depend create_way_members_table
--test-depend create_relation_members
--test-depend create_relation_attributes
--test-param

DELETE FROM MemberWays WHERE NOT EXISTS(SELECT r.ID FROM Relations r WHERE r.ID=MemberWays.RelationID)

--name delete_orphan7
--type psql::BindTypes<>, psql::RetTypes<>
--test-depend create_import_seq
--test-depend create_import_table
--test-depend create_nodes_table
--test-depend create_ways_table
--test-depend create_relations_table
--test-depend create_node_attributes
--test-depend create_node_members
--test-depend create_waynodes_table
--test-depend create_way_attributes
--test-depend create_way_members_table
--test-depend create_relation_members
--test-depend create_relation_attributes
--test-param

DELETE FROM MemberRelations WHERE NOT EXISTS(SELECT r.ID FROM Relations r WHERE r.ID=MemberRelations.ParentID)

--name delete_duplicit_import_nodes
--type psql::BindTypes<>, psql::RetTypes<>
--test-depend create_import_seq
--test-depend create_import_table
--test-depend create_nodes_table
--test-depend create_ways_table
--test-depend create_relations_table
--test-depend create_node_attributes
--test-depend create_node_members
--test-depend create_waynodes_table
--test-depend create_way_attributes
--test-depend create_way_members_table
--test-depend create_relation_members
--test-depend create_relation_attributes
--test-param

DELETE FROM Import WHERE EntryType = 1 AND EXISTS(SELECT i.biginta FROM Import i WHERE i.ID<Import.ID AND i.EntryType=1 AND i.biginta = Import.biginta)

--name delete_duplicit_import_ways
--type psql::BindTypes<>, psql::RetTypes<>
--test-depend create_import_seq
--test-depend create_import_table
--test-depend create_nodes_table
--test-depend create_ways_table
--test-depend create_relations_table
--test-depend create_node_attributes
--test-depend create_node_members
--test-depend create_waynodes_table
--test-depend create_way_attributes
--test-depend create_way_members_table
--test-depend create_relation_members
--test-depend create_relation_attributes
--test-param

DELETE FROM Import WHERE EntryType = 2 AND EXISTS(SELECT i.biginta FROM Import i WHERE i.ID<Import.ID AND i.EntryType=2 AND i.biginta = Import.biginta)

--name delete_duplicit_import_rels
--type psql::BindTypes<>, psql::RetTypes<>
--test-depend create_import_seq
--test-depend create_import_table
--test-depend create_nodes_table
--test-depend create_ways_table
--test-depend create_relations_table
--test-depend create_node_attributes
--test-depend create_node_members
--test-depend create_waynodes_table
--test-depend create_way_attributes
--test-depend create_way_members_table
--test-depend create_relation_members
--test-depend create_relation_attributes
--test-param

DELETE FROM Import WHERE EntryType = 3 AND EXISTS(SELECT i.biginta FROM Import i WHERE i.ID<Import.ID AND i.EntryType=3 AND i.biginta = Import.biginta)

--name delete_incomplete_ways
--type psql::BindTypes<>, psql::RetTypes<>
--test-depend create_import_seq
--test-depend create_import_table
--test-depend create_nodes_table
--test-depend create_ways_table
--test-depend create_relations_table
--test-depend create_node_attributes
--test-depend create_node_members
--test-depend create_waynodes_table
--test-depend create_way_attributes
--test-depend create_way_members_table
--test-depend create_relation_members
--test-depend create_relation_attributes
--test-param

DELETE FROM Import WHERE EntryType = 2 AND EXISTS(
											SELECT i.biginta FROM Import i WHERE i.EntryType = 7 AND i.biginta = Import.biginta 
												AND NOT EXISTS(SELECT n.ID FROM Nodes n WHERE i.bigintb=n.ID) 
												AND NOT EXISTS(SELECT i2.biginta FROM Import i2 where i2.biginta = i.bigintb AND i2.EntryType=1)
										)

--name delete_incomplete_rels1
--type psql::BindTypes<>, psql::RetTypes<>
--test-depend create_import_seq
--test-depend create_import_table
--test-depend create_nodes_table
--test-depend create_ways_table
--test-depend create_relations_table
--test-depend create_node_attributes
--test-depend create_node_members
--test-depend create_waynodes_table
--test-depend create_way_attributes
--test-depend create_way_members_table
--test-depend create_relation_members
--test-depend create_relation_attributes
--test-param

DELETE FROM Import WHERE EntryType = 3 AND EXISTS(
											SELECT i.biginta FROM Import i WHERE i.biginta = Import.biginta AND i.EntryType = 8
												AND NOT EXISTS(SELECT n.ID FROM Nodes n WHERE i.bigintb=n.ID) 
												AND NOT EXISTS(SELECT i2.biginta FROM Import i2 where i2.biginta = i.bigintb AND i2.EntryType=1)
										)

--name delete_incomplete_rels2
--type psql::BindTypes<>, psql::RetTypes<>
--test-depend create_import_seq
--test-depend create_import_table
--test-depend create_nodes_table
--test-depend create_ways_table
--test-depend create_relations_table
--test-depend create_node_attributes
--test-depend create_node_members
--test-depend create_waynodes_table
--test-depend create_way_attributes
--test-depend create_way_members_table
--test-depend create_relation_members
--test-depend create_relation_attributes
--test-param
										
DELETE FROM Import WHERE EntryType = 3 AND EXISTS(
											SELECT i.biginta FROM Import i WHERE i.biginta = Import.biginta AND i.EntryType = 9
												AND NOT EXISTS(SELECT w.ID FROM Ways w WHERE i.bigintb=w.ID) 
												AND NOT EXISTS(SELECT i2.biginta FROM Import i2 where i2.biginta = i.bigintb AND i2.EntryType=2)
										)

--name delete_incomplete_rels3
--type psql::BindTypes<>, psql::RetTypes<>
--test-depend create_import_seq
--test-depend create_import_table
--test-depend create_nodes_table
--test-depend create_ways_table
--test-depend create_relations_table
--test-depend create_node_attributes
--test-depend create_node_members
--test-depend create_waynodes_table
--test-depend create_way_attributes
--test-depend create_way_members_table
--test-depend create_relation_members
--test-depend create_relation_attributes
--test-param
										
DELETE FROM Import WHERE EntryType = 3 AND EXISTS(
											SELECT i.biginta FROM Import i WHERE i.biginta = Import.biginta AND i.EntryType = 10
												AND NOT EXISTS(SELECT r.ID FROM Relations r WHERE i.bigintb=r.ID) 
												AND NOT EXISTS(SELECT i2.biginta FROM Import i2 where i2.biginta = i.bigintb AND i2.EntryType=3)
										)

--name delete_orphan_import1
--type psql::BindTypes<>, psql::RetTypes<>
--test-depend create_import_seq
--test-depend create_import_table
--test-depend create_nodes_table
--test-depend create_ways_table
--test-depend create_relations_table
--test-depend create_node_attributes
--test-depend create_node_members
--test-depend create_waynodes_table
--test-depend create_way_attributes
--test-depend create_way_members_table
--test-depend create_relation_members
--test-depend create_relation_attributes
--test-param

DELETE FROM Import WHERE EntryType = 4 AND NOT EXISTS(SELECT i.biginta FROM Import i WHERE i.biginta=Import.biginta AND i.EntryType=1)

--name delete_orphan_import2
--type psql::BindTypes<>, psql::RetTypes<>
--test-depend create_import_seq
--test-depend create_import_table
--test-depend create_nodes_table
--test-depend create_ways_table
--test-depend create_relations_table
--test-depend create_node_attributes
--test-depend create_node_members
--test-depend create_waynodes_table
--test-depend create_way_attributes
--test-depend create_way_members_table
--test-depend create_relation_members
--test-depend create_relation_attributes
--test-param

DELETE FROM Import WHERE EntryType = 5 AND NOT EXISTS(SELECT i.biginta FROM Import i WHERE i.biginta=Import.biginta AND i.EntryType=2)

--name delete_orphan_import3
--type psql::BindTypes<>, psql::RetTypes<>
--test-depend create_import_seq
--test-depend create_import_table
--test-depend create_nodes_table
--test-depend create_ways_table
--test-depend create_relations_table
--test-depend create_node_attributes
--test-depend create_node_members
--test-depend create_waynodes_table
--test-depend create_way_attributes
--test-depend create_way_members_table
--test-depend create_relation_members
--test-depend create_relation_attributes
--test-param

DELETE FROM Import WHERE EntryType = 6 AND NOT EXISTS(SELECT i.biginta FROM Import i WHERE i.biginta=Import.biginta AND i.EntryType=3)

--name delete_orphan_import4
--type psql::BindTypes<>, psql::RetTypes<>
--test-depend create_import_seq
--test-depend create_import_table
--test-depend create_nodes_table
--test-depend create_ways_table
--test-depend create_relations_table
--test-depend create_node_attributes
--test-depend create_node_members
--test-depend create_waynodes_table
--test-depend create_way_attributes
--test-depend create_way_members_table
--test-depend create_relation_members
--test-depend create_relation_attributes
--test-param
										
DELETE FROM Import WHERE EntryType = 7 AND NOT EXISTS(SELECT i.biginta FROM Import i WHERE i.biginta = Import.biginta AND i.EntryType = 2)

--name delete_orphan_import5
--type psql::BindTypes<>, psql::RetTypes<>
--test-depend create_import_seq
--test-depend create_import_table
--test-depend create_nodes_table
--test-depend create_ways_table
--test-depend create_relations_table
--test-depend create_node_attributes
--test-depend create_node_members
--test-depend create_waynodes_table
--test-depend create_way_attributes
--test-depend create_way_members_table
--test-depend create_relation_members
--test-depend create_relation_attributes
--test-param

DELETE FROM Import WHERE EntryType = 8 AND NOT EXISTS(SELECT i.biginta FROM Import i WHERE i.biginta = Import.biginta AND i.EntryType = 3)

--name delete_orphan_import6
--type psql::BindTypes<>, psql::RetTypes<>
--test-depend create_import_seq
--test-depend create_import_table
--test-depend create_nodes_table
--test-depend create_ways_table
--test-depend create_relations_table
--test-depend create_node_attributes
--test-depend create_node_members
--test-depend create_waynodes_table
--test-depend create_way_attributes
--test-depend create_way_members_table
--test-depend create_relation_members
--test-depend create_relation_attributes
--test-param

DELETE FROM Import WHERE EntryType = 9 AND NOT EXISTS(SELECT i.biginta FROM Import i WHERE i.biginta = Import.biginta AND i.EntryType = 3)

--name delete_orphan_import7
--type psql::BindTypes<>, psql::RetTypes<>
--test-depend create_import_seq
--test-depend create_import_table
--test-depend create_nodes_table
--test-depend create_ways_table
--test-depend create_relations_table
--test-depend create_node_attributes
--test-depend create_node_members
--test-depend create_waynodes_table
--test-depend create_way_attributes
--test-depend create_way_members_table
--test-depend create_relation_members
--test-depend create_relation_attributes
--test-param

DELETE FROM Import WHERE EntryType = 10 AND NOT EXISTS(SELECT i.biginta FROM Import i WHERE i.biginta = Import.biginta AND i.EntryType = 3)

--name do_import1
--type psql::BindTypes<>, psql::RetTypes<>
--test-depend create_import_seq
--test-depend create_import_table
--test-depend create_nodes_table
--test-depend create_ways_table
--test-depend create_relations_table
--test-depend create_node_attributes
--test-depend create_node_members
--test-depend create_waynodes_table
--test-depend create_way_attributes
--test-depend create_way_members_table
--test-depend create_relation_members
--test-depend create_relation_attributes
--test-param
										
INSERT INTO Nodes (ID, Location) SELECT biginta, ST_MakePoint(doublea, doubleb) FROM Import WHERE EntryType = 1

--name do_import2
--type psql::BindTypes<>, psql::RetTypes<>
--test-depend create_import_seq
--test-depend create_import_table
--test-depend create_nodes_table
--test-depend create_ways_table
--test-depend create_relations_table
--test-depend create_node_attributes
--test-depend create_node_members
--test-depend create_waynodes_table
--test-depend create_way_attributes
--test-depend create_way_members_table
--test-depend create_relation_members
--test-depend create_relation_attributes
--test-param

INSERT INTO Ways (ID) SELECT biginta FROM Import WHERE EntryType=2

--name do_import3
--type psql::BindTypes<>, psql::RetTypes<>
--test-depend create_import_seq
--test-depend create_import_table
--test-depend create_nodes_table
--test-depend create_ways_table
--test-depend create_relations_table
--test-depend create_node_attributes
--test-depend create_node_members
--test-depend create_waynodes_table
--test-depend create_way_attributes
--test-depend create_way_members_table
--test-depend create_relation_members
--test-depend create_relation_attributes
--test-param

INSERT INTO Relations (ID) SELECT biginta FROM Import WHERE EntryType=3

--name do_import4
--type psql::BindTypes<>, psql::RetTypes<>
--test-depend create_import_seq
--test-depend create_import_table
--test-depend create_nodes_table
--test-depend create_ways_table
--test-depend create_relations_table
--test-depend create_node_attributes
--test-depend create_node_members
--test-depend create_waynodes_table
--test-depend create_way_attributes
--test-depend create_way_members_table
--test-depend create_relation_members
--test-depend create_relation_attributes
--test-param

INSERT INTO NodeAttributes (NodeID, Key, Value) SELECT biginta, stringa, stringb FROM Import WHERE EntryType=4

--name do_import5
--type psql::BindTypes<>, psql::RetTypes<>
--test-depend create_import_seq
--test-depend create_import_table
--test-depend create_nodes_table
--test-depend create_ways_table
--test-depend create_relations_table
--test-depend create_node_attributes
--test-depend create_node_members
--test-depend create_waynodes_table
--test-depend create_way_attributes
--test-depend create_way_members_table
--test-depend create_relation_members
--test-depend create_relation_attributes
--test-param

INSERT INTO WayAttributes (WayID, Key, Value) SELECT biginta, stringa, stringb FROM Import WHERE EntryType=5

--name do_import6
--type psql::BindTypes<>, psql::RetTypes<>
--test-depend create_import_seq
--test-depend create_import_table
--test-depend create_nodes_table
--test-depend create_ways_table
--test-depend create_relations_table
--test-depend create_node_attributes
--test-depend create_node_members
--test-depend create_waynodes_table
--test-depend create_way_attributes
--test-depend create_way_members_table
--test-depend create_relation_members
--test-depend create_relation_attributes
--test-param

INSERT INTO RelationAttributes (RelationID, Key, Value) SELECT biginta, stringa, stringb FROM Import WHERE EntryType=6

--name do_import7
--type psql::BindTypes<>, psql::RetTypes<>
--test-depend create_import_seq
--test-depend create_import_table
--test-depend create_nodes_table
--test-depend create_ways_table
--test-depend create_relations_table
--test-depend create_node_attributes
--test-depend create_node_members
--test-depend create_waynodes_table
--test-depend create_way_attributes
--test-depend create_way_members_table
--test-depend create_relation_members
--test-depend create_relation_attributes
--test-param

INSERT INTO WayNodes (WayID, NodeID, SequenceNo) SELECT biginta, bigintb, inta FROM Import WHERE EntryType = 7

--name do_import8
--type psql::BindTypes<>, psql::RetTypes<>
--test-depend create_import_seq
--test-depend create_import_table
--test-depend create_nodes_table
--test-depend create_ways_table
--test-depend create_relations_table
--test-depend create_node_attributes
--test-depend create_node_members
--test-depend create_waynodes_table
--test-depend create_way_attributes
--test-depend create_way_members_table
--test-depend create_relation_members
--test-depend create_relation_attributes
--test-param

INSERT INTO MemberNodes (RelationID, Role, NodeID) SELECT biginta, stringa, bigintb FROM Import WHERE EntryType = 8

--name do_import9
--type psql::BindTypes<>, psql::RetTypes<>
--test-depend create_import_seq
--test-depend create_import_table
--test-depend create_nodes_table
--test-depend create_ways_table
--test-depend create_relations_table
--test-depend create_node_attributes
--test-depend create_node_members
--test-depend create_waynodes_table
--test-depend create_way_attributes
--test-depend create_way_members_table
--test-depend create_relation_members
--test-depend create_relation_attributes
--test-param

INSERT INTO MemberWays (RelationID, Role, WayID) SELECT biginta, stringa, bigintb FROM Import WHERE EntryType = 9

--name do_import10
--type psql::BindTypes<>, psql::RetTypes<>
--test-depend create_import_seq
--test-depend create_import_table
--test-depend create_nodes_table
--test-depend create_ways_table
--test-depend create_relations_table
--test-depend create_node_attributes
--test-depend create_node_members
--test-depend create_waynodes_table
--test-depend create_way_attributes
--test-depend create_way_members_table
--test-depend create_relation_members
--test-depend create_relation_attributes
--test-param

INSERT INTO MemberRelations (ParentID, Role, ChildID) SELECT biginta, stringa, bigintb FROM Import WHERE EntryType = 10

--name do_import11
--type psql::BindTypes<>, psql::RetTypes<>
--test-depend create_import_seq
--test-depend create_import_table
--test-depend create_nodes_table
--test-depend create_ways_table
--test-depend create_edges_table
--test-depend create_relations_table
--test-depend create_node_attributes
--test-depend create_node_members
--test-depend create_waynodes_table
--test-depend create_way_attributes
--test-depend create_way_members_table
--test-depend create_relation_members
--test-depend create_relation_attributes
--test-param

INSERT INTO Edges (WayID, StartNodeID, EndNodeID, Location)
    SELECT i.biginta, i.bigintb, i2.bigintb, ST_MakeLine(n1.Location::geometry, n2.Location::geometry)::geography FROM
        Import i INNER JOIN Import i2 ON i2.biginta = i.biginta AND i.inta + 1 = i2.inta INNER JOIN Nodes n1 ON n1.ID = i.bigintb INNER JOIN Nodes n2 ON n2.ID = i2.bigintb    
            WHERE i.EntryType=7 AND i2.EntryType=7

--name clear_import_table
--type psql::BindTypes<>, psql::RetTypes<>
--test-depend create_import_seq
--test-depend create_import_table
--test-param

DELETE FROM Import
