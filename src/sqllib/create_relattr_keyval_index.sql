--type psql::BindTypes<>, psql::RetTypes<>
--test-depend create_relation_attributes
--test-param

CREATE INDEX IX_RelAttr_KeyVal ON RelationAttributes (Key, Value)

