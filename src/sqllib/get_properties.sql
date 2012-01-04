--name select_node_attributes
--type psql::BindTypes<int64_t>, psql::RetTypes<std::string, std::string>
--test-depend create_node_attributes
--test-depend insert_node_attr 10, "asdf", "bdas"
--test-param 10
--test-result 0

SELECT Key, Value from NodeAttributes WHERE NodeID = $1

--name select_way_attributes
--type psql::BindTypes<int64_t>, psql::RetTypes<std::string, std::string>
--test-depend create_way_attributes
--test-depend insert_way_attr 10, "asdf", "bdas"
--test-param 10
--test-result 0

SELECT Key, Value from WayAttributes WHERE WayID = $1

--name select_waynodes
--type psql::BindTypes<int64_t>, psql::RetTypes<int64_t>
--test-depend create_waynodes_table
--test-depend insert_way_node 10, 12, 1
--test-param 10
--test-result 0

SELECT NodeID FROM WayNodes WHERE WayID = $1 ORDER BY SequenceNo

--name select_rel_attributes
--type psql::BindTypes<int64_t>, psql::RetTypes<std::string, std::string>
--test-depend create_relation_attributes
--test-depend insert_rel_attribute 10, "asdf", "gfads"
--test-param 10
--test-result 0

SELECT Key, Value FROM RelationAttributes WHERE RelationID = $1

--name select_node_members
--type psql::BindTypes<int64_t>, psql::RetTypes<std::string, int64_t>
--test-depend create_node_members
--test-depend insert_node_member 10, "asdf", 30
--test-param 10
--test-result 0

SELECT Role, NodeID FROM MemberNodes WHERE RelationID = $1

--name select_way_members
--type psql::BindTypes<int64_t>, psql::RetTypes<std::string, int64_t>
--test-depend create_way_members_table
--test-depend insert_way_member 10, "asdf", 30
--test-param 10
--test-result 0

SELECT Role, WayID FROM MemberWays WHERE RelationID = $1

--name select_rel_members
--type psql::BindTypes<int64_t>, psql::RetTypes<std::string, int64_t>
--test-depend create_relation_members
--test-depend insert_relation_member 10, "asdf", 30
--test-param 10
--test-result 0

SELECT Role, ChildID FROM MemberRelations WHERE ParentID = $1

--name select_position
--type psql::BindTypes<int64_t>, psql::RetTypes<double, double>
--test-depend create_nodes_table
--test-depend insert_node 10, 12.3, 13.5
--test-param 10
--test-result 0

SELECT ST_X(Location::geometry), ST_Y(Location::geometry) FROM Nodes WHERE ID = $1

