--type psql::BindTypes<>, psql::RetTypes<>
--test-depend create_node_attributes
--test-depend create_nodeattributes_pkey
--test-param

ALTER TABLE NodeAttributes DROP CONSTRAINT PK_NodeAttributes

