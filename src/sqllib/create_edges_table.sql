--type psql::BindTypes<>, psql::RetTypes<>
--test-param

CREATE TABLE Edges (
    WayID bigint, 
    StartNodeID bigint, 
    EndNodeID bigint, 
    StartSequenceNo int,
    EndSequenceNo int,
    Location geography(LINESTRING, 4326)
    )

--name create_edges_start_waynode_fkey
--type psql::BindTypes<>, psql::RetTypes<>
--test-depend create_edges_table
--test-depend create_waynodes_table
--test-depend create_waynodes_pkey
--test-param

ALTER TABLE Edges ADD CONSTRAINT FK_StartWayNode FOREIGN KEY (WayID, StartSequenceNo) REFERENCES WayNodes (WayID, SequenceNo)

--name create_edges_end_waynode_fkey
--type psql::BindTypes<>, psql::RetTypes<>
--test-depend create_edges_table
--test-depend create_waynodes_table
--test-depend create_waynodes_pkey
--test-param

ALTER TABLE Edges ADD CONSTRAINT FK_EndWayNode FOREIGN KEY (WayID, EndSequenceNo) REFERENCES WayNodes (WayID, SequenceNo)

--name drop_edges_start_waynode_fkey
--type psql::BindTypes<>, psql::RetTypes<>
--test-depend create_edges_table
--test-depend create_waynodes_table
--test-depend create_waynodes_pkey
--test-depend create_edges_start_waynode_fkey
--test-param

ALTER TABLE Edges DROP CONSTRAINT FK_StartWayNode

--name drop_edges_end_waynode_fkey
--type psql::BindTypes<>, psql::RetTypes<>
--test-depend create_edges_table
--test-depend create_waynodes_table
--test-depend create_waynodes_pkey
--test-depend create_edges_end_waynode_fkey
--test-param

ALTER TABLE Edges DROP CONSTRAINT FK_EndWayNode

