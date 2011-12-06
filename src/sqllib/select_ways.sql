--type psql::BindTypes<>, psql::RetTypes<int64_t>
--test-depend create_ways_table
--test-depend insert_way 20
--test-param
--test-result 0

SELECT ID FROM Ways;

