--type psql::BindTypes<>, psql::RetTypes<>
--test-depend create_relations_table
--test-depend create_relations_pkey
--test-depend create_relation_attributes
--test-depend create_relationattributes_relation_fkey
--test-param

ALTER TABLE RelationAttributes DROP CONSTRAINT FK_RelationAttributes_Relations

