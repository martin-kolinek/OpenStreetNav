add_executable(tests test.cpp sqlite.cpp osmdb.cpp xml.cpp projection.cpp)

target_link_libraries(tests projection xmlparse sqlitewrap osmdb ${Boost_TEST_EXEC_MONITOR_LIBRARY} ${Boost_SYSTEM_LIBRARY} ${Boost_FILESYSTEM_LIBRARY})

add_test(TestSuite ${EXECUTABLE_OUTPUT_PATH}/tests)
