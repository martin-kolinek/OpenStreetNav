--type psql::BindTypes<>, psql::RetTypes<>
--test-depend create_edges_table
--test-param

ALTER TABLE Edges ADD CONSTRAINT PK_Edges PRIMARY KEY (WayID, StartSequenceNo)

