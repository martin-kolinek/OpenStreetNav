--type psql::BindTypes<>, psql::RetTypes<>
--test-depend create_edges_table
--test-depend create_edges_pkey
--test-param

ALTER TABLE Edges DROP CONSTRAINT PK_Edges

