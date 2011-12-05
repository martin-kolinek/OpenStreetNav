--type psql::BindTypes<>, psql::RetTypes<>
--test-depend create_waynodes_table
--test-depend create_nodes_table
--test-depend create_nodes_pkey
--test-depend create_waynodes_node_fkey
--test-param

ALTER TABLE WayNodes DROP CONSTRAINT FK_WayNodes_Node

