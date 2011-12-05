--type psql::BindTypes<>, psql::RetTypes<>
--test-depend create_ways_table
--test-depend create_waynodes_table
--test-depend create_ways_pkey
--test-depend create_waynodes_way_fkey
--test-param

ALTER TABLE WayNodes DROP CONSTRAINT FK_WayNodes_Way

