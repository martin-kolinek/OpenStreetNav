--type psql::BindTypes<>, psql::RetTypes<>
--test-depend create_waynodes_table
--test-depend create_nodes_table
--test-depend create_nodes_pkey
--test-param

ALTER TABLE WayNodes ADD CONSTRAINT FK_WayNodes_Node FOREIGN KEY (NodeID) REFERENCES Nodes (ID)

