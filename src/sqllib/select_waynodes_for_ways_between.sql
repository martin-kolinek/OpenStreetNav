--type psql::BindTypes<int64_t, int64_t>, psql::RetTypes<int64_t, int64_t, double>
--test-depend create_ways_table
--test-depend create_waynodes_table
--test-depend create_nodes_table
--test-depend create_edges_table
--test-depend insert_node 21, 0, 0
--test-depend insert_node 22, 0, 1
--test-depend insert_way 20
--test-depend insert_way_node 20, 21, 1, 2
--test-depend insert_way_node 20, 22, 2, -1
--test-depend insert_edge 20, 21, 22, 1, 2
--test-param 10, 30
--test-result 0
--test-result 1
              
select w.WayID, w.NodeID, coalesce(ST_Length(e.Location,false), 0) FROM WayNodes w left join Edges e on e.wayid=w.wayid and e.startnodeid = w.nodeid where w.WayID >=$1 and w.WayID<$2 order by w.wayid
