--type psql::BindTypes<>, psql::RetTypes<>
--test-depend create_ways_table
--test-depend create_waynodes_table
--test-depend create_waynodes_pkey
--test-param

ALTER TABLE WayNodes DROP CONSTRAINT PK_WayNodes

