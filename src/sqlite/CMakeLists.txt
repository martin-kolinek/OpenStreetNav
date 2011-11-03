find_package(PkgConfig)

pkg_check_modules(SQLITE3 REQUIRED sqlite3)

link_directories(${SQLITE3_LIBRARY_DIRS} )

include_directories(${SQLITE3_INCLUDE_DIRS})

add_library(sqlitewrap Statement.cpp Database.cpp InvalidUseException.cpp SqliteException.cpp)

target_link_libraries(sqlitewrap ${SQLITE3_LIBRARIES})

