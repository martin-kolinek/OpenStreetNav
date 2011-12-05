--type psql::BindTypes<>, psql::RetTypes<>
--test-depend create_ways_table
--test-param

ALTER TABLE Ways ADD CONSTRAINT PK_Ways PRIMARY KEY (ID)

