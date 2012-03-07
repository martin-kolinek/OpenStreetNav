--type psql::BindTypes<>, psql::RetTypes<int64_t>
--test-depend create_ways_table
--test-depend insert_way 20
--test-param
--test-result 0

SELECT ID FROM Ways;

--name select_ways_with_nodes
--type psql::BindTypes<>, psql::RetTypes<int64_t, int64_t, int>
--test-depend create_ways_table
--test-depend create_waynodes_table
--test-depend insert_way 20
--test-depend insert_way_node 20, 10, 1
--test-param
--test-result 0

SELECT WayID, NodeID, SequenceNo FROM WayNodes ORDER BY WayID, SequenceNo
