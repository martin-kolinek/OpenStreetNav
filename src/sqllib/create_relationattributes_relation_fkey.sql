--type psql::BindTypes<>, psql::RetTypes<>
--test-depend create_relations_table
--test-depend create_relation_attributes
--test-depend create_relations_pkey
--test-param

ALTER TABLE RelationAttributes ADD CONSTRAINT FK_RelationAttributes_Relations FOREIGN KEY (RelationID) REFERENCES Relations (ID)

