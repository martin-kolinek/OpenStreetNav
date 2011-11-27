--type psql::BindTypes<int64_t>, psql::RetTypes<>
--test-depend create_ways_table
--test-param 20

INSERT INTO Ways (ID) VALUES ($1)

