--type psql::BindTypes<int, double, double, double, double>, psql::RetTypes<double, double, double, double>
--test-depend create_ways_table
--test-depend create_nodes_table
--test-depend create_edges_table
--test-depend insert_way 41
--test-depend insert_node 10, 25, 40
--test-depend insert_node 11, 20, 30
--test-depend insert_edge 41, 10, 11, 0, 1
--test-depend create_toshow_table
--test-depend create_way_attributes
--test-depend insert_way_attr 41, "asdf", "asdf"
--test-depend insert_toshow "asdf", "asdf", 5
--test-param 5, 0, 0, 50, 50
--test-result 0

SELECT ST_X(ST_StartPoint(e.Location::geometry)), ST_Y(ST_StartPoint(e.Location::geometry)), ST_X(ST_EndPoint(e.Location::geometry)), ST_Y(ST_EndPoint(e.Location::geometry)) FROM
    Edges e INNER JOIN WayAttributes a ON e.WayID = a.WayID INNER JOIN ToShow t ON t.Key = a.Key AND t.Value=a.Value WHERE
    t.Zoom=$1 AND e.Location && ST_SetSRID(ST_MakeBox2D(ST_MakePoint($2, $3), ST_MakePoint($4, $5)), -1)
