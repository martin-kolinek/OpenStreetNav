--type psql::BindTypes<>, psql::RetTypes<>
--test-depend create_way_members_table
--test-param

ALTER TABLE MemberWays ADD CONSTRAINT PK_WayMembers PRIMARY KEY (RelationID, Role, WayID)

