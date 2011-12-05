--type psql::BindTypes<>, psql::RetTypes<>
--test-param

CREATE TABLE Edges (
    WayID bigint, 
    StartNodeID bigint, 
    EndNodeID bigint, 
    Location geography(LINESTRING, 4326)
    )
