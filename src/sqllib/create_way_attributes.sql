--type psql::BindTypes<>, psql::RetTypes<>
--test-depend create_ways_table
--test-param

CREATE TABLE WayAttributes (
    WayID bigint REFERENCES Ways (ID),
    Key text,
    Value text,
    PRIMARY KEY (WayID, Key)
    )

