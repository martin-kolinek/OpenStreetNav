--name create_road_edges_table
--type psql::BindTypes<>, psql::RetTypes<>
--test-param

CREATE TABLE RoadEdges (
    WayID bigint,
    SequenceNo int,
    Forward boolean,
    Length float8,
    Time float8
)

--name create_road_edges_view
--type psql::BindTypes<>, psql::RetTypes<>
--test-depend create_road_edges_table
--test-depend create_edges_table
--test-param

CREATE VIEW ViewRoadEdges AS
    SELECT e.StartNodeID, e.EndNodeID, r.Length, r.Time FROM RoadEdges r INNER JOIN Edges e ON e.WayID = r.WayID AND r.SequenceNo = e.SequenceNo AND r.Forward = TRUE
    UNION
    SELECT e.EndNodeID, e.StartNodeID, r.Length, r.Time FROM RoadEdges r INNER JOIN Edges e ON e.WayID = r.WayID AND r.SequenceNo = e.SequenceNo AND r.Forward = FALSE

--name create_road_edges_pkey
--type psql::BindTypes<>, psql::RetTypes<>
--test-depend create_road_edges_table
--test-param

ALTER TABLE RoadEdges ADD CONSTRAINT PK_RoadEdges PRIMARY KEY (WayID, SequenceNo, Forward)

--name drop_road_edges_pkey
--type psql::BindTypes<>, psql::RetTypes<>
--test-depend create_road_edges_table
--test-depend create_road_edges_pkey
--test-param

ALTER TABLE RoadEdges DROP CONSTRAINT PK_RoadEdges

--name create_road_edges_fkey
--type psql::BindTypes<>, psql::RetTypes<>
--test-depend create_road_edges_table
--test-depend create_edges_table
--test-depend create_edges_pkey
--test-param

ALTER TABLE RoadEdges ADD CONSTRAINT FK_RoadEdgesEdges FOREIGN KEY (WayID, SequenceNo) REFERENCES Edges (WayID, SequenceNo)

--name drop_road_edges_fkey
--type psql::BindTypes<>, psql::RetTypes<>
--test-depend create_road_edges_table
--test-depend create_edges_table
--test-depend create_edges_pkey
--test-depend create_road_edges_fkey
--test-param

ALTER TABLE RoadEdges DROP CONSTRAINT FK_RoadEdgesEdges


