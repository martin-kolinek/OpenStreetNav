add_executable(sqlitetest main.cc)
target_link_libraries(sqlitetest sqlitewrap ${Boost_SYSTEM_LIBRARY} ${Boost_FILESYSTEM_LIBRARY})