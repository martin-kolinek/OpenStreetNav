--name create_road_edges_table
--type psql::BindTypes<>, psql::RetTypes<>
--test-param

CREATE TABLE RoadEdges (
    WayID bigint,
    StartNodeID bigint,
    EndNodeID bigint,
    StartSequenceNo int,
    EndSequenceNo int,
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
    SELECT n1.ID as StartNodeID, n1.Location as StartNodeLocation,
           n2.ID as EndNodeID, n2.Location as EndNodeLocation,
           r.WayID as WayID, r.StartSequenceNo as StartSequenceNo, r.EndSequenceNo as EndSequenceNo, r.Forward as Forward, r.Cost as Cost FROM RoadEdges r 
        INNER JOIN Nodes n1 ON r.StartNodeID = n1.ID
        INNER JOIN Nodes n2 ON r.EndNodeID = n2.ID
            WHERE r.Forward = TRUE
    UNION
    SELECT n1.ID as StartNodeID, n1.Location as StartNodeLocation,
           n2.ID as EndNodeID, n2.Location as EndNodeLocation,
           r.WayID as WayID, r.StartSequenceNo as StartSequenceNo, r.EndSequenceNo as EndSequenceNo, r.Forward as Forward, r.Cost as Cost FROM RoadEdges r  
        INNER JOIN Nodes n1 ON r.EndNodeID = n1.ID 
        INNER JOIN Nodes n2 ON r.StartNodeID = n2.ID 
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
--type psql::BindTypes<>, psql::RetTypes<>, psql::CopyTypes<int64_t, int64_t, int64_t, int, int, bool, double>
--test-depend create_road_edges_table
--test-param

COPY RoadEdges (WayID, StartNodeID, EndNodeID, StartSequenceNo, EndSequenceNo, Forward, Cost) FROM STDIN

--name insert_road_edge
--type psql::BindTypes<int64_t, int64_t, int64_t, int, int, bool, double>, psql::RetTypes<>
--test-depend create_road_edges_table
--test-param 1, 2, 1, 0, 1, true, 1.0

INSERT INTO RoadEdges(WayID, StartNodeID, EndNodeID, StartSequenceNo, EndSequenceNo, Forward, Cost) VALUES ($1, $2, $3, $4, $5, $6, $7)
                  
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
--test-depend insert_road_edge 1, 2, 1, 0, 1, 1, 1
--test-param
--test-result 0
                            
SELECT 
    StartNodeID, ST_X(StartNodeLocation::geometry), ST_Y(StartNodeLocation::geometry), 
    EndNodeID, ST_X(EndNodeLocation::geometry), ST_Y(EndNodeLocation::geometry), 
    WayID, StartSequenceNo, EndSequenceNo, Forward, Cost FROM ViewRoadEdges
       
