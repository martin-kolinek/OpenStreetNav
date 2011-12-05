--type psql::BindTypes<>, psql::RetTypes<>
--test-depend create_node_members
--test-param

ALTER TABLE MemberNodes ADD CONSTRAINT PK_NodeMembers PRIMARY KEY (RelationID, Role, NodeID)

