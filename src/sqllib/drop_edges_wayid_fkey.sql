--type psql::BindTypes<>, psql::RetTypes<>
--test-depend create_edges_table
--test-depend create_ways_table
--test-depend create_ways_pkey
--test-depend create_edges_wayid_fkey
--test-param

ALTER TABLE Edges DROP CONSTRAINT FK_Edges_Way

