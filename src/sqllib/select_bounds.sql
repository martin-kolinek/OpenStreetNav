--type psql::BindTypes<>, psql::RetTypes<double, double, double, double>
--test-depend create_nodes_table
--test-depend insert_node 123, 10, 20
--test-depend create_bounds_table
--test-depend insert_bounds
--test-param
--test-result 0

select LeftBound, RightBound, TopBound, BottomBound FROM Bounds

--name create_bounds_table
--type psql::BindTypes<>, psql::RetTypes<>
--test-param

CREATE TABLE Bounds
    (
        LeftBound float8 not null,
        RightBound float8 not null,
        TopBound float8 not null,
        BottomBound float8 not null
    )    

--name insert_bounds
--type psql::BindTypes<>, psql::RetTypes<>
--test-depend create_bounds_table
--test-depend create_nodes_table
--test-param

INSERT INTO Bounds (LeftBound, RightBound, TopBound, BottomBound) select COALESCE(min(ST_X(Location::geometry)), 0), COALESCE(max(ST_X(Location::geometry)), 0), COALESCE(min(ST_Y(Location::geometry)), 0), COALESCE(max(ST_Y(Location::geometry)), 0) FROM Nodes

--name delete_bounds
--type psql::BindTypes<>, psql::RetTypes<>
--test-depend create_bounds_table
--test-param

DELETE FROM Bounds
