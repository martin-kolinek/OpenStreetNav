--type psql::BindTypes<int64_t, std::string, std::string>, psql::RetTypes<>
--test-depend create_nodes_table
--test-depend create_node_attributes
--test-depend insert_node 10, 20, 30
--test-param 10, "asdf", "bdas"

INSERT INTO NodeAttributes (NodeID, Key, Value) VALUES ($1, $2, $3)

