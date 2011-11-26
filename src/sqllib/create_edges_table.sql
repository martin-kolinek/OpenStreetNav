--type psql::BindTypes<>, psql::RetTypes<>
--test-depend create_ways_table
--test-depend create_nodes_table
--test-param

CREATE TABLE Edges (
    WayID bigint REFERENCES Ways (ID), 
    StartNodeID bigint REFERENCES Nodes (ID), 
    EndNodeID bigint REFERENCES Nodes (ID), 
    Location geography(LINESTRING, 4326),
    PRIMARY KEY (WayID, StartNodeID, EndNodeID)
    )
