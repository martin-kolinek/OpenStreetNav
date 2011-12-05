--type psql::BindTypes<>, psql::RetTypes<>
--test-depend create_way_members_table
--test-depend create_ways_table
--test-depend create_ways_pkey
--test-param

ALTER TABLE MemberWays ADD CONSTRAINT FK_WayMembers_Way FOREIGN KEY (WayID) REFERENCES Ways (ID)

