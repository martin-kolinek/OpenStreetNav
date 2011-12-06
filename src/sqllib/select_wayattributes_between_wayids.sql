--type psql::BindTypes<int64_t, int64_t>, psql::RetTypes<int64_t, std::string, std::string>
--test-depend create_ways_table
--test-depend create_way_attributes
--test-depend insert_way 20
--test-depend insert_way_attr 20, "asdf", "asdf"
--test-param 10, 30
--test-result 0

select wayid, key, value from wayattributes where wayid>=$1 and wayid<$2 order by wayid
                            