--type psql::BindTypes<>, psql::RetTypes<>
--test-depend create_relation_attributes
--test-depend create_relationattributes_pkey
--test-param

ALTER TABLE RelationAttributes DROP CONSTRAINT PK_RelationAttributes

