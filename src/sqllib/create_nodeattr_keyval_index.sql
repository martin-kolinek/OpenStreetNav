--type psql::BindTypes<>, psql::RetTypes<>
--test-depend create_node_attributes
--test-param

CREATE INDEX IX_NodeAttr_KeyVal ON NodeAttributes (Key, Value)

