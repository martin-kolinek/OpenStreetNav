--type psql::BindTypes<int, std::string, int64_t>, psql::RetTypes<>
--test-depend create_test_table
--test-param 10, "gda", 34422354323
INSERT INTO TestTable (A, B, C) VALUES ($1, $2, $3)
