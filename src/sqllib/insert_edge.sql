--type psql::BindTypes<int64_t, int64_t, int64_t, int>, psql::RetTypes<>
--test-depend create_ways_table
--test-depend create_nodes_table
--test-depend create_edges_table
--test-depend insert_way 41
--test-depend insert_node 10, 25, 40
--test-depend insert_node 11, 20, 30
--test-param 41, 10, 11, 0

INSERT INTO Edges (WayID, StartNodeID, EndNodeID, SequenceNo, Location)
    SELECT $1, $2, $3, $4, ST_MakeLine(n1.Location::geometry, n2.Location::geometry)::geography FROM Nodes n1, Nodes n2 WHERE n1.ID=$2 AND n2.ID=$3

