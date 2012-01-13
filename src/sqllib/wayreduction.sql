--name decl_curs_way_reduction
--type psql::BindTypes<>, psql::RetTypes<>
--test-depend create_waynodes_table
--test-depend create_nodes_table

DECLARE CURSOR wayredcrs FOR
    SELECT wn.WayID, ST_X(n.Location::geometry), ST_Y(n.Location::geometry), COUNT(DISTINCT wn2.WayID) FROM WayNodes wn INNER JOIN 
        Nodes n ON wn.NodeID = n.ID INNER JOIN 
        WayNodes wn2 ON wn2.WayID != wn.WayID AND wn.NodeID = wn2.NodeID
    ORDER BY wn.WayID, wn.SequenceNo

--name open_curs_way_reduction
--type psql::BindTypes<>, psql::RetTypes<>
--test-depend create_waynodes_table
--test-depend create_nodes_table
--test-depend decl_curs_way_reduction

OPEN CURSOR wayredcrs

--name fetch_curs_way_reduction
--type psql::BindTypes<>, psql::RetTypes<int64_t, double, double, int>
