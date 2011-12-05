--type psql::BindTypes<>, psql::RetTypes<>
--test-depend create_way_attributes
--test-depend create_wayattributes_pkey
--test-param

ALTER TABLE WayAttributes DROP CONSTRAINT PK_WayAttributes

