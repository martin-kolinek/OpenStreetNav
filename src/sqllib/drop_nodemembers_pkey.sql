--type psql::BindTypes<>, psql::RetTypes<>
--test-depend create_node_members
--test-depend create_nodemembers_pkey
--test-param

ALTER TABLE MemberNodes DROP CONSTRAINT PK_NodeMembers

