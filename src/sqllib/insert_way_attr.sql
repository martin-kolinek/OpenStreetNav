--type psql::BindTypes<int64_t, std::string, std::string>, psql::RetTypes<>
--test-depend create_ways_table
--test-depend create_way_attributes
--test-depend insert_way 30
--test-param 30, "afsa", "gdas"

INSERT INTO WayAttributes (WayID, Key, Value) VALUES ($1, $2, $3)

