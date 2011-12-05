--type psql::BindTypes<>, psql::RetTypes<>
--test-depend create_node_members
--test-depend create_nodes_table
--test-depend create_nodes_pkey
--test-depend create_nodemembers_node_fkey
--test-param

ALTER TABLE MemberNodes DROP CONSTRAINT FK_NodeMembers_Node

