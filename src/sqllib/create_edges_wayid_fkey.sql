--type psql::BindTypes<>, psql::RetTypes<>
--test-depend create_edges_table
--test-depend create_ways_table
--test-depend create_ways_pkey
--test-param

ALTER TABLE Edges ADD CONSTRAINT FK_Edges_Way FOREIGN KEY (WayID) REFERENCES Ways (ID)

