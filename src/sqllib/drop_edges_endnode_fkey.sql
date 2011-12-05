--type psql::BindTypes<>, psql::RetTypes<>
--test-depend create_edges_table
--test-depend create_nodes_table
--test-depend create_nodes_pkey
--test-depend create_edges_endnode_fkey
--test-param

ALTER TABLE Edges DROP CONSTRAINT FK_Edges_EndNode

