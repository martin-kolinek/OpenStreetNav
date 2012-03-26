add_executable(edgecreate main.cc)

SET(TOSHOW_PATH ${CMAKE_BINARY_DIR}/share/toshow/)
add_custom_target(display_to_show ${CMAKE_COMMAND} -E copy_directory ${CMAKE_CURRENT_SOURCE_DIR}/to_show_edges ${TOSHOW_PATH}to_show_edges)

target_link_libraries(edgecreate osmdb ${Boost_PROGRAM_OPTIONS_LIBRARY} ${Boost_SYSTEM_LIBRARY} ${Boost_FILESYSTEM_LIBRARY} ${Boost_REGEX_LIBRARY})