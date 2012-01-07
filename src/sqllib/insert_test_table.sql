--type psql::BindTypes<int, std::string, int64_t>, psql::RetTypes<>
--test-depend create_test_table
--test-param 10, "gda", 34422354323
INSERT INTO TestTable (A, B, C) VALUES ($1, $2, $3)

--name copy_test_table
--type psql::BindTypes<>, psql::RetTypes<>, psql::CopyTypes<int, std::string, int64_t>
--test-depend create_test_table
--test-param

COPY TestTable(A, B, C) FROM STDIN

