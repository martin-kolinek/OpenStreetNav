--name insert_relation 
--type psql::BindTypes<int64_t>, psql::RetTypes<>
--test-depend create_relations_table
--test-param 10

INSERT INTO Relations (ID) VALUES ($1)

--name insert_rel_attribute
--type psql::BindTypes<int64_t, std::string, std::string>, psql::RetTypes<>
--test-depend create_relation_attributes
--test-param 10, "asdf", "gfads"

INSERT INTO RelationAttributes (RelationID, Key, Value) VALUES ($1, $2, $3)

--name insert_node_member
--type psql::BindTypes<int64_t, std::string, int64_t>, psql::RetTypes<>
--test-depend create_node_members
--test-param 10, "asdf", 30

INSERT INTO MemberNodes (RelationID, Role, NodeID) VALUES ($1, $2, $3)

--name insert_way_member
--type psql::BindTypes<int64_t, std::string, int64_t>, psql::RetTypes<>
--test-depend create_way_members_table
--test-param 10, "asdf", 30

INSERT INTO MemberWays (RelationID, Role, WayID) VALUES ($1, $2, $3)

--name insert_relation_member
--type psql::BindTypes<int64_t, std::string, int64_t>, psql::RetTypes<>
--test-depend create_relation_members
--test-param 10, "asdf", 30

INSERT INTO MemberRelations (ParentID, Role, ChildID) VALUES ($1, $2, $3)
