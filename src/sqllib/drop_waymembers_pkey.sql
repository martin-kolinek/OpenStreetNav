--type psql::BindTypes<>, psql::RetTypes<>
--test-depend create_way_members_table
--test-depend create_waymembers_pkey
--test-param

ALTER TABLE MemberWays DROP CONSTRAINT PK_WayMembers

