--type psql::BindTypes<>, psql::RetTypes<>
--test-depend create_relations_table
--test-depend create_nodes_table
--test-param

CREATE TABLE MemberNodes (
    RelationID bigint REFERENCES Relations (ID),
    Role text,
    NodeID bigint REFERENCES Nodes (ID),
    PRIMARY KEY (RelationID, Role, NodeID)
    )

