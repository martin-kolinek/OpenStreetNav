add_executable(tests test.cpp sqlite.cpp osmdb.cpp xml.cpp projection.cpp util.cpp psql.cpp sqllib_test)

target_link_libraries(tests util projection wkb psql xmlparse sqllib sqlitewrap osmdb ${Boost_TEST_EXEC_MONITOR_LIBRARY} ${Boost_SYSTEM_LIBRARY} ${Boost_FILESYSTEM_LIBRARY})

add_test(TestSuite ${EXECUTABLE_OUTPUT_PATH}/tests)
