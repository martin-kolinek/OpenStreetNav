--type psql::BindTypes<>, psql::RetTypes<>
--test-depend create_waynodes_table
--test-depend create_ways_table
--test-depend create_ways_pkey
--test-param

ALTER TABLE WayNodes ADD CONSTRAINT FK_WayNodes_Way FOREIGN KEY (WayID) REFERENCES Ways (ID)

