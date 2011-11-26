--type psql::BindTypes<>, psql::RetTypes<>
--test-depend create_ways_table
--test-depend create_nodes_table
--test-param

CREATE TABLE WayNodes (
    WayID bigint REFERENCES Ways (ID),
    NodeID bigint REFERENCES Nodes (ID),
    SequenceNo int,
    PRIMARY KEY (WayID, SequenceNo)
    )
