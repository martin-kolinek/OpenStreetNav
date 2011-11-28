--type psql::BindTypes<>, psql::RetTypes<>
--test-param

CREATE TABLE ToShow (
    Key text, 
    Value text, 
    Zoom int,
    PRIMARY KEY (Key, Value, Zoom)
)

