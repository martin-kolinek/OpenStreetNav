--type psql::BindTypes<>, psql::RetTypes<>
--test-depend create_way_attributes
--test-param

ALTER TABLE WayAttributes ADD CONSTRAINT PK_WayAttributes PRIMARY KEY (WayID, Key)

