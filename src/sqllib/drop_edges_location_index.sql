--type psql::BindTypes<>, psql::RetTypes<>
--test-depend create_edges_table
--test-depend create_edges_location_index
--test-param

DROP INDEX IX_EdgesLocation

