--type psql::BindTypes<>, psql::RetTypes<>
--test-depend create_node_members
--test-depend create_relations_table
--test-depend create_relations_pkey
--test-depend create_nodemembers_relation_fkey
--test-param

ALTER TABLE MemberNodes DROP CONSTRAINT FK_NodeMembers_Relation

