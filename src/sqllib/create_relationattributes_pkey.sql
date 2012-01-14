--type psql::BindTypes<>, psql::RetTypes<>
--test-depend create_relation_attributes
--test-param

ALTER TABLE RelationAttributes ADD CONSTRAINT PK_RelationAttributes PRIMARY KEY (RelationID, Key, Value)

