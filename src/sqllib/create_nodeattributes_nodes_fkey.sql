--type psql::BindTypes<>, psql::RetTypes<>
--test-depend create_nodes_table
--test-depend create_node_attributes
--test-depend create_nodes_pkey
--test-param

ALTER TABLE NodeAttributes ADD CONSTRAINT FK_NodeAttributes_Nodes FOREIGN KEY (NodeID) REFERENCES Nodes (ID)

