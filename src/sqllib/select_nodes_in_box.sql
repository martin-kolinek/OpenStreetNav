--type psql::BindTypes<int, double, double, double, double>, psql::RetTypes<double, double>
--test-depend create_nodes_table
--test-depend insert_node 10, 25, 40
--test-depend create_toshow_table
--test-depend create_node_attributes
--test-depend insert_node_attr 10, "asdf", "asdf"
--test-depend insert_toshow "asdf", "asdf", 5
--test-param 5, 0, 0, 50, 50

SELECT ST_X(n.Location::geometry), ST_Y(n.Location::geometry) FROM
    Nodes n INNER JOIN NodeAttributes a ON n.ID = a.NodeID INNER JOIN ToShow t ON t.Key = a.Key AND t.Value=a.Value WHERE
    t.Zoom=$1 AND n.Location && ST_SetSRID(ST_MakeBox2D(ST_MakePoint($2, $3), ST_MakePoint($4, $5)), -1)
