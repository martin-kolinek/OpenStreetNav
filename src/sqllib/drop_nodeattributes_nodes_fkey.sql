--type psql::BindTypes<>, psql::RetTypes<>
--test-depend create_nodes_table
--test-depend create_node_attributes
--test-depend create_nodes_pkey
--test-depend create_nodeattributes_nodes_fkey
--test-param

ALTER TABLE NodeAttributes DROP CONSTRAINT FK_NodeAttributes_Nodes

