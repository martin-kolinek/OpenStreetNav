--type psql::BindTypes<>, psql::RetTypes<>
--test-param

CREATE TABLE Edges (
    WayID bigint, 
    StartNodeID bigint, 
    EndNodeID bigint, 
    SequenceNo int,
    Location geography(LINESTRING, 4326)
    )
