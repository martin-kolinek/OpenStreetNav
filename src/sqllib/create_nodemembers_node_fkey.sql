--type psql::BindTypes<>, psql::RetTypes<>
--test-depend create_node_members
--test-depend create_nodes_table
--test-depend create_nodes_pkey
--test-param

ALTER TABLE MemberNodes ADD CONSTRAINT FK_NodeMembers_Node FOREIGN KEY (NodeID) REFERENCES Nodes (ID)

