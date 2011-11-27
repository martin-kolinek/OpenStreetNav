--type psql::BindTypes<>, psql::RetTypes<>
--test-depend create_relations_table
--test-param

CREATE TABLE RelationAttributes (
    RelationID bigint REFERENCES Relations (ID),
    Key text,
    Value text,
    PRIMARY KEY (RelationID, Key)
    )

