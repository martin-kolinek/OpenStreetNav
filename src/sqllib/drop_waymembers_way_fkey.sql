--type psql::BindTypes<>, psql::RetTypes<>
--test-depend create_way_members_table
--test-depend create_ways_table
--test-depend create_ways_pkey
--test-depend create_waymembers_way_fkey
--test-param

ALTER TABLE MemberWays DROP CONSTRAINT FK_WayMembers_Way

