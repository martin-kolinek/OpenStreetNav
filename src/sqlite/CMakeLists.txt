add_library(sqlitewrap Statement.cpp Database.cpp InvalidUseException.cpp SqliteException.cpp)

target_link_libraries(sqlitewrap ${SQLITE3_LIBRARIES})

