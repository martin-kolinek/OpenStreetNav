--type psql::BindTypes<>, psql::RetTypes<>
--test-depend create_nodes_table
--test-param

ALTER TABLE Nodes ADD CONSTRAINT PK_Nodes PRIMARY KEY (ID)

