--type psql::BindTypes<>, psql::RetTypes<>
--test-depend create_relations_table
--test-depend create_relations_pkey
--test-param

ALTER TABLE Relations DROP CONSTRAINT PK_Relations

