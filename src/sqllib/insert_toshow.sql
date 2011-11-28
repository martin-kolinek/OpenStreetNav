--type psql::BindTypes<std::string, std::string, int>, psql::RetTypes<>
--test-depend create_toshow_table
--test-param "asdf", "asdf", 5

INSERT INTO ToShow(Key, Value, Zoom) VALUES ($1, $2, $3)

