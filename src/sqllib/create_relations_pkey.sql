--type psql::BindTypes<>, psql::RetTypes<>
--test-depend create_relations_table
--test-param

ALTER TABLE Relations ADD CONSTRAINT PK_Relations PRIMARY KEY (ID)

