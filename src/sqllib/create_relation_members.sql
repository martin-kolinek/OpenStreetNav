--name create_relation_members
--type psql::BindTypes<>, psql::RetTypes<>
--test-param 

CREATE TABLE MemberRelations (ParentID bigint, Role text, ChildID bigint)

--name create_relmembers_parent_fkey
--type psql::BindTypes<>, psql::RetTypes<>
--test-depend create_relation_members
--test-depend create_relations_table
--test-depend create_relations_pkey
--test-param 

ALTER TABLE MemberRelations ADD CONSTRAINT FK_MemberRelations_Parent FOREIGN KEY (ParentID) REFERENCES Relations (ID)

--name create_relmembers_child_fkey
--type psql::BindTypes<>, psql::RetTypes<>
--test-depend create_relation_members
--test-depend create_relations_table
--test-depend create_relations_pkey
--test-param 

ALTER TABLE MemberRelations ADD CONSTRAINT FK_MemberRelations_Child FOREIGN KEY (ChildID) REFERENCES Relations (ID)

--name create_relmembers_pkey
--type psql::BindTypes<>, psql::RetTypes<>
--test-depend create_relation_members

ALTER TABLE MemberRelations ADD CONSTRAINT PK_MemberRelations PRIMARY KEY (ParentID, Role, ChildID)

--name drop_relmembers_pkey
--type psql::BindTypes<>, psql::RetTypes<>
--test-depend create_relation_members
--test-depend create_relmembers_pkey

ALTER TABLE MemberRelations DROP CONSTRAINT PK_MemberRelations

--name drop_relmembers_parent_fkey
--type psql::BindTypes<>, psql::RetTypes<>
--test-depend create_relations_table
--test-depend create_relations_pkey
--test-depend create_relation_members
--test-depend create_relmembers_parent_fkey

ALTER TABLE MemberRelations DROP CONSTRAINT FK_MemberRelations_Parent

--name drop_relmembers_child_fkey
--type psql::BindTypes<>, psql::RetTypes<>
--test-depend create_relations_table
--test-depend create_relations_pkey
--test-depend create_relation_members
--test-depend create_relmembers_child_fkey

ALTER TABLE MemberRelations DROP CONSTRAINT FK_MemberRelations_Child

