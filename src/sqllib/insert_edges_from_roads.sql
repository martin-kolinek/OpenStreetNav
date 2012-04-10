--type psql::BindTypes<>, psql::RetTypes<>
--test-depend create_road_edges_table
--test-depend create_edges_table
--test-depend insert_road_edge 1, 2, 1, 0, 1, 1, 2, 2, 2, true, 1
--test-param

INSERT INTO Edges (WayID, StartSequenceNo, EndSequenceNo, StartNodeID, EndNodeID, Red, Green, Blue, Alpha, Thickness, Style, Priority, Location)
    SELECT r.WayID, r.StartSequenceNo, r.EndSequenceNo, MIN(r.StartNodeID), MIN(r.EndNodeID), 
        1, 
        CASE WHEN ST_Distance(ST_MakePoint(MIN(r.StartLon), MIN(r.StartLat))::geography, ST_MakePoint(MIN(r.EndLon), MIN(r.EndLat))::geography) < 0.000001 THEN 0
            ELSE (ST_Distance(ST_MakePoint(MIN(r.StartLon), MIN(r.StartLat))::geography, ST_MakePoint(MIN(r.EndLon), MIN(r.EndLat))::geography)/(MIN(r.Cost)*1000))^2 END,
        CASE WHEN ST_Distance(ST_MakePoint(MIN(r.StartLon), MIN(r.StartLat))::geography, ST_MakePoint(MIN(r.EndLon), MIN(r.EndLat))::geography) < 0.000001 THEN 0
            ELSE (ST_Distance(ST_MakePoint(MIN(r.StartLon), MIN(r.StartLat))::geography, ST_MakePoint(MIN(r.EndLon), MIN(r.EndLat))::geography)/(MIN(r.Cost)*1000))^2 END, 
        1, 1,
        CASE WHEN BOOL_OR(Forward) AND NOT BOOL_AND(Forward) THEN 0 WHEN BOOL_OR(Forward) THEN 1 ELSE 2 END, 
        1, 
        ST_MakeLine(ST_MakePoint(MIN(r.StartLon), MIN(r.StartLat)), ST_MakePoint(MIN(r.EndLon), MIN(r.EndLat)))
            FROM RoadEdges r GROUP BY r.WayID, r.StartSequenceNo, r.EndSequenceNo

