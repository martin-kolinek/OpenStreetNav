--type psql::BindTypes<>, psql::RetTypes<>
--test-depend create_way_members_table
--test-depend create_relations_table
--test-depend create_relations_pkey
--test-depend create_waymembers_relation_fkey
--test-param

ALTER TABLE MemberWays DROP CONSTRAINT FK_WayMembers_Relation

