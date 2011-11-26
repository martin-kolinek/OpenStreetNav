--type psql::BindTypes<>, psql::RetTypes<>
--test-depend create_nodes_table
--test-param

CREATE TABLE NodeAttributes (
    NodeID bigint REFERENCES Nodes (ID),
    Key text,
    Value text,
    PRIMARY KEY (NodeID, Key)
    )

