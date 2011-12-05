--type psql::BindTypes<>, psql::RetTypes<>
--test-depend create_ways_table
--test-depend create_way_attributes
--test-depend create_ways_pkey
--test-depend create_wayattributes_ways_fkey
--test-param

ALTER TABLE WayAttributes DROP CONSTRAINT FK_WayAttributes_Ways

