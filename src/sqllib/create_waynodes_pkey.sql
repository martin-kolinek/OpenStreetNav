--type psql::BindTypes<>, psql::RetTypes<>
--test-depend create_waynodes_table
--test-param

ALTER TABLE WayNodes ADD CONSTRAINT PK_WayNodes PRIMARY KEY (WayID, SequenceNo)

