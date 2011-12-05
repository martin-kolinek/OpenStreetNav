--type psql::BindTypes<>, psql::RetTypes<>
--test-depend create_way_attributes
--test-param

CREATE INDEX IX_WayAttr_KeyVal ON WayAttributes (Key, Value)

