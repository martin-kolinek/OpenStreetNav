--type psql::BindTypes<>, psql::RetTypes<>
--test-depend create_ways_table
--test-depend create_ways_pkey
--test-param

ALTER TABLE Ways DROP CONSTRAINT PK_Ways

