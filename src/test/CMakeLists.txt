add_executable(tests test.cpp sqlite.cpp osmdb.cpp xml.cpp projection.cpp util.cpp psql.cpp sqllib2.cpp sqllib.cpp osmelements.cpp wayreduction_test.cpp pathfinding.cpp display.cpp cost.cpp)
add_executable(select_ways_test main.cc)
SET(TOSHOW_TEST_PATH ${CMAKE_BINARY_DIR}/share/toshow/)
file(COPY ${CMAKE_CURRENT_SOURCE_DIR}/testtoshow DESTINATION ${TOSHOW_TEST_PATH})

configure_file(${CMAKE_CURRENT_SOURCE_DIR}/testconf.h.in ${CMAKE_BINARY_DIR}/include/test_config.h)

target_link_libraries(tests util cost projection wkb psql xmlparse sqllib sqlitewrap wayred osmdb ${Boost_TEST_EXEC_MONITOR_LIBRARY} ${Boost_SYSTEM_LIBRARY} ${Boost_FILESYSTEM_LIBRARY} ${Boost_REGEX_LIBRARY})
target_link_libraries(select_ways_test psql osmdb)

add_custom_target(test_to_show ${CMAKE_COMMAND} -E copy_directory ${CMAKE_CURRENT_SOURCE_DIR}/testtoshow ${TOSHOW_TEST_PATH}/testtoshow)  

add_dependencies(tests sqllib_gtest test_to_show)
add_test(TestSuite ${EXECUTABLE_OUTPUT_PATH}/tests)
