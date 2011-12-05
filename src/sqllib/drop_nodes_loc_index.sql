--type psql::BindTypes<>, psql::RetTypes<>
--test-depend create_nodes_table
--test-depend create_nodes_loc_index
--test-param

DROP INDEX IX_NodesLoc

