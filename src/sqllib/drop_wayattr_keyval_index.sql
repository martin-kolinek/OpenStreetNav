--type psql::BindTypes<>, psql::RetTypes<>
--test-depend create_way_attributes
--test-depend create_wayattr_keyval_index
--test-param

DROP INDEX IX_WayAttr_KeyVal

