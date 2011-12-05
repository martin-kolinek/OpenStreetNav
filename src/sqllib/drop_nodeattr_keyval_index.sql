--type psql::BindTypes<>, psql::RetTypes<>
--test-depend create_node_attributes
--test-depend create_nodeattr_keyval_index
--test-param

DROP INDEX IX_NodeAttr_KeyVal

