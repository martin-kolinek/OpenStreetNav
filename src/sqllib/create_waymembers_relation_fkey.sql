--type psql::BindTypes<>, psql::RetTypes<>
--test-depend create_way_members_table
--test-depend create_relations_table
--test-depend create_relations_pkey
--test-param

ALTER TABLE MemberWays ADD CONSTRAINT FK_WayMembers_Relation FOREIGN KEY (RelationID) REFERENCES Relations (ID)

