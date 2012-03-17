--type psql::BindTypes<int64_t, int64_t, int, int>, psql::RetTypes<>
--test-depend create_ways_table
--test-depend create_nodes_table
--test-depend create_waynodes_table
--test-depend insert_node 21, 30, 40
--test-depend insert_way 20
--test-param 20, 21, 1, -1

INSERT INTO WayNodes (WayID, NodeID, SequenceNo, NextSequenceNo) VALUES ($1, $2, $3, NULLIF($4, -1))

