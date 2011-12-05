--type psql::BindTypes<>, psql::RetTypes<>
--test-depend create_ways_table
--test-depend create_way_attributes
--test-depend create_ways_pkey
--test-param

ALTER TABLE WayAttributes ADD CONSTRAINT FK_WayAttributes_Ways FOREIGN KEY (WayID) REFERENCES Ways (ID)

