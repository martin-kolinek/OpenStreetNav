add_executable(tests test.cpp sqlite.cpp)

target_link_libraries(tests sqlitewrap ${Boost_TEST_EXEC_MONITOR_LIBRARY} ${Boost_SYSTEM_LIBRARY} ${Boost_FILESYSTEM_LIBRARY})

add_test(TestSuite ${EXECUTABLE_OUTPUT_PATH}/tests)
