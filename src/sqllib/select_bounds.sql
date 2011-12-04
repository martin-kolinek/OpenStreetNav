--type psql::BindTypes<>, psql::RetTypes<double, double, double, double>
--test-depend create_nodes_table
--test-depend insert_node 123, 10, 20
--test-param
--test-result 0

select min(ST_X(Location::geometry)), max(ST_X(Location::geometry)), min(ST_Y(Location::geometry)), max(ST_Y(Location::geometry)) FROM Nodes

