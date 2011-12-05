--type psql::BindTypes<>, psql::RetTypes<>
--test-depend create_edges_table
--test-depend create_nodes_table
--test-depend create_nodes_pkey
--test-param

ALTER TABLE Edges ADD CONSTRAINT FK_Edges_EndNode FOREIGN KEY (EndNodeID) REFERENCES Nodes (ID)

