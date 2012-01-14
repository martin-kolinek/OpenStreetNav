--type psql::BindTypes<>, psql::RetTypes<>
--test-depend create_node_attributes
--test-param

ALTER TABLE NodeAttributes ADD CONSTRAINT PK_NodeAttributes PRIMARY KEY (NodeID, Key, Value)

