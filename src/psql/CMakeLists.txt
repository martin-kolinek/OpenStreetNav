add_library(psql PgSqlException.cpp Database.cpp)

target_link_libraries(psql ${PostgreSQL_LIBRARIES})