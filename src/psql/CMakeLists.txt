add_library(psql PgSqlException.cpp Database.cpp Statement.cpp RetTypes.cpp PqTypeWrap.cpp)

target_link_libraries(psql ${PostgreSQL_LIBRARIES})