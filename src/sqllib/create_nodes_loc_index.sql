--type psql::BindTypes<>, psql::RetTypes<>
--test-depend create_nodes_table
--test-param

CREATE INDEX IX_NodesLoc ON Nodes USING GIST (Location)

