add_executable(tests test.cpp sqlite.cpp osmdb.cpp xml.cpp projection.cpp util.cpp psql.cpp sqllib2.cpp sqllib.cpp)

target_link_libraries(tests util projection wkb psql xmlparse sqllib sqlitewrap osmdb ${Boost_TEST_EXEC_MONITOR_LIBRARY} ${Boost_SYSTEM_LIBRARY} ${Boost_FILESYSTEM_LIBRARY} ${Boost_REGEX_LIBRARY})
add_dependencies(tests sqllib_gtest)
add_test(TestSuite ${EXECUTABLE_OUTPUT_PATH}/tests)
