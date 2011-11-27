--type psql::BindTypes<int64_t, double, double>, psql::RetTypes<>
--test-depend create_nodes_table
--test-param 123, 10, 20

INSERT INTO Nodes (ID, Location) VALUES ($1, ST_MakePoint($2, $3))

