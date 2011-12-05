--type psql::BindTypes<>, psql::RetTypes<>
--test-depend create_node_members
--test-depend create_relations_table
--test-depend create_relations_pkey
--test-param

ALTER TABLE MemberNodes ADD CONSTRAINT FK_NodeMembers_Relation FOREIGN KEY (RelationID) REFERENCES Relations (ID)

