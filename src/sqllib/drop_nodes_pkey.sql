--type psql::BindTypes<>, psql::RetTypes<>
--test-depend create_nodes_table
--test-depend create_nodes_pkey
--test-param

ALTER TABLE Nodes DROP CONSTRAINT PK_Nodes

