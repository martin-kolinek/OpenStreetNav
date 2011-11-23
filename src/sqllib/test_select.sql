-- type psql::BindTypes<int>, psql::RetTypes<int>
-- test-depend create_test_table
-- test-depend insert_test_table 10, "asdf", 20000000000
-- test-param 10
-- test-result 0

SELECT * FROM TestTable
	WHERE
--comment
 A = $1;

