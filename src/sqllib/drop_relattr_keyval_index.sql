--type psql::BindTypes<>, psql::RetTypes<>
--test-depend create_relation_attributes
--test-depend create_relattr_keyval_index
--test-param

DROP INDEX IX_RelAttr_KeyVal

