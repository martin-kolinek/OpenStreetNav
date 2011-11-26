--type psql::BindTypes<>, psql::RetTypes<>
--test-depend create_nodes_table
--test-depend create_ways_table
--test-depend create_edges_table
--test-param

CREATE INDEX IX_EdgesLocation ON Edges USING GIST (Location)

