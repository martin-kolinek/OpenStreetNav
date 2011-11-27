--type psql::BindTypes<>, psql::RetTypes<>
--test-depend create_relations_table
--test-depend create_ways_table
--test-param

CREATE TABLE MemberWays (
    RelationID bigint REFERENCES Relations (ID),
    Role text,
    WayID bigint REFERENCES Ways (ID),
    PRIMARY KEY (RelationID, Role, WayID)
    )

