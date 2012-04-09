--type psql::BindTypes<double, double, double, double>, psql::RetTypes<int, int64_t, double, double, int, int64_t, double, double, int64_t, double, double, double, double, double, int, int> 
--test-depend create_nodes_table
--test-depend create_edges_table
--test-depend insert_node 10, 25, 40
--test-depend insert_node 11, 20, 30
--test-depend insert_edge 41, 10, 11, 0, 1, 1, 1, 1, 1, 0, 0
--test-param 0, 0, 50, 50
--test-result 0

SELECT StartSequenceNo, StartNodeID, ST_X(ST_StartPoint(Location::geometry)), ST_Y(ST_StartPoint(Location::geometry)), 
        EndSequenceNo, EndNodeID, ST_X(ST_EndPoint(Location::geometry)), ST_Y(ST_EndPoint(Location::geometry)),
        WayID, Red, Green, Blue, Alpha, Thickness, Style, Priority  FROM
    Edges WHERE Location && ST_SetSRID(ST_MakeBox2D(ST_MakePoint($1::float8, $2::float8), ST_MakePoint($3::float8, $4::float8)), -1) ORDER BY Priority DESC

--name select_edges_in_exact_box
--type psql::BindTypes<double, double, double, double>, psql::RetTypes<int64_t, int, int64_t, int, int64_t> 
--test-depend create_nodes_table
--test-depend create_edges_table
--test-depend insert_node 10, 25, 40
--test-depend insert_node 11, 20, 30
--test-depend insert_edge 41, 10, 11, 0, 1, 1, 1, 1, 1, 0, 0
--test-param 0, 0, 50, 50
--test-result 0

SELECT WayID, StartSequenceNo, StartNodeID, EndSequenceNo, EndNodeID FROM Edges WHERE
    ST_Intersects(Location, ST_SetSRID(ST_MakeBox2D(ST_MakePoint($1::float8, $2::float8), ST_MakePoint($3::float8, $4::float8)), -1)) ORDER BY Priority DESC

