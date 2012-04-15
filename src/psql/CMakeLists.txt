add_library(psql PgSqlException.cpp Database.cpp Statement.cpp RetTypes.cpp PqTypeWrap.cpp BindTypes.cpp psql.cpp CopyTypes.cpp StatementParams.cpp)

target_link_libraries(psql util ${POSTGRESQL_LIBRARIES})