--type psql::BindTypes<>, psql::RetTypes<>
--test-depend create_edges_table
--test-depend create_nodes_table
--test-depend create_waynodes_table
--test-param

INSERT INTO Edges (WayID, StartNodeID, StartSequenceNo, EndNodeID, EndSequenceNo, Location)
    SELECT wn1.WayID, n1.ID, wn1.SequenceNo, n2.ID, wn2.SequenceNo, ST_MakeLine(n1.Location::geometry, n2.Location::geometry)::geography
        FROM 
            WayNodes wn1 INNER JOIN 
            WayNodes wn2 ON wn1.WayID = wn2.WayID AND wn1.NextSequenceNo=wn2.SequenceNo INNER JOIN
            Nodes n1 ON wn1.NodeID = n1.ID INNER JOIN 
            Nodes n2 ON wn2.NodeID = n2.ID



