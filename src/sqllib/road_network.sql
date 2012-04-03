--name create_road_edges_table
--type psql::BindTypes<>, psql::RetTypes<>
--test-param

CREATE TABLE RoadEdges (
    WayID bigint,
    StartNodeID bigint,
    EndNodeID bigint,
    StartSequenceNo int,
    EndSequenceNo int,
    StartLon float8,
    StartLat float8,
    EndLon float8,
    EndLat float8,
    Forward boolean,
    Cost float8
)

--name create_road_edges_view
--type psql::BindTypes<>, psql::RetTypes<>
--test-depend create_road_edges_table
--test-depend create_waynodes_table
--test-depend create_nodes_table
--test-param

CREATE VIEW ViewRoadEdges AS
    SELECT r.StartNodeID as StartNodeID, r.StartLon as StartLon, r.StartLat as StartLat,
           r.EndNodeID as EndNodeID, r.EndLon as EndLon, r.EndLat as EndLat,
           r.WayID as WayID, r.StartSequenceNo as StartSequenceNo, r.EndSequenceNo as EndSequenceNo, r.Forward as Forward, r.Cost as Cost FROM RoadEdges r 
            WHERE r.Forward = TRUE
    UNION ALL
    SELECT r.EndNodeID as StartNodeID, r.EndLon as StartLon, r.EndLat as StartLat,
           r.StartNodeID as EndNodeID, r.StartLon as EndLon, r.StartLat as EndLat,
           r.WayID as WayID, r.StartSequenceNo as StartSequenceNo, r.EndSequenceNo as EndSequenceNo, r.Forward as Forward, r.Cost as Cost FROM RoadEdges r  
            WHERE r.Forward = FALSE 

--name create_road_edges_pkey
--type psql::BindTypes<>, psql::RetTypes<>
--test-depend create_road_edges_table
--test-param

ALTER TABLE RoadEdges ADD CONSTRAINT PK_RoadEdges PRIMARY KEY (WayID, StartSequenceNo, Forward)

--name drop_road_edges_pkey
--type psql::BindTypes<>, psql::RetTypes<>
--test-depend create_road_edges_table
--test-depend create_road_edges_pkey
--test-param

ALTER TABLE RoadEdges DROP CONSTRAINT PK_RoadEdges

--name create_road_edges_fkey
--type psql::BindTypes<>, psql::RetTypes<>
--test-depend create_road_edges_table
--test-depend create_waynodes_table
--test-depend create_waynodes_pkey
--test-param

ALTER TABLE RoadEdges ADD CONSTRAINT FK_RoadEdgesEdges FOREIGN KEY (WayID, StartSequenceNo) REFERENCES WayNodes (WayID, SequenceNo)

--name drop_road_edges_fkey
--type psql::BindTypes<>, psql::RetTypes<>
--test-depend create_road_edges_table
--test-depend create_waynodes_table
--test-depend create_waynodes_pkey
--test-depend create_road_edges_fkey
--test-param

ALTER TABLE RoadEdges DROP CONSTRAINT FK_RoadEdgesEdges

--name copy_road_network
--type psql::BindTypes<>, psql::RetTypes<>, psql::CopyTypes<int64_t, int64_t, int64_t, int, int, double, double, double, double, bool, double>
--test-depend create_road_edges_table
--test-param

COPY RoadEdges (WayID, StartNodeID, EndNodeID, StartSequenceNo, EndSequenceNo, StartLon, StartLat, EndLon, EndLat, Forward, Cost) FROM STDIN

--name insert_road_edge
--type psql::BindTypes<int64_t, int64_t, int64_t, int, int, double, double, double, double, bool, double>, psql::RetTypes<>
--test-depend create_road_edges_table
--test-param 1, 2, 1, 0, 1, 1, 0, 2, 1, true, 1.0

INSERT INTO RoadEdges(WayID, StartNodeID, EndNodeID, StartSequenceNo, EndSequenceNo, StartLon, StartLat, EndLon, EndLat, Forward, Cost) 
    VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11)
                  
--name select_road_edges
--type psql::BindTypes<>, psql::RetTypes<int64_t, double, double, int64_t, double, double, int64_t, int, int, bool, double>
--test-depend create_road_edges_table
--test-depend create_waynodes_table
--test-depend create_nodes_table
--test-depend create_road_edges_view
--test-depend insert_node 1, 1, 2
--test-depend insert_node 2, 2, 2
--test-depend insert_way_node 1, 2, 0, 1
--test-depend insert_way_node 1, 1, 1, -1
--test-depend insert_road_edge 1, 2, 1, 0, 1, 1, 2, 2, 2, true, 1
--test-param
--test-result 0
                            
SELECT 
    StartNodeID, StartLon, StartLat,
    EndNodeID, EndLon, EndLat,
    WayID, StartSequenceNo, EndSequenceNo, Forward, Cost FROM ViewRoadEdges
       
