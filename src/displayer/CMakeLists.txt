add_library(displaylib MapDrawingArea.cpp DisplayElement.cpp DisplayStyle.cpp LineDisplayStyle.cpp DisplayLine.cpp DisplayProvider.cpp EdgeHighlightable.cpp EdgeHighlighter.cpp Descriptible.cpp)
add_executable(displayer main.cc)
configure_file(display.glade ${CMAKE_BINARY_DIR}/share/display.glade COPYONLY)
SET(GLADE_PATH ${CMAKE_BINARY_DIR}/share/display.glade)
SET(TOSHOW_PATH ${CMAKE_BINARY_DIR}/share/toshow/)
configure_file(${CMAKE_SOURCE_DIR}/src/displayer/config.h.in ${CMAKE_BINARY_DIR}/include/osmdisp_config.h)

add_custom_target(display_to_show ${CMAKE_COMMAND} -E copy_directory ${CMAKE_CURRENT_SOURCE_DIR}/to_show_edges ${TOSHOW_PATH}to_show_edges)

target_link_libraries(displaylib geoelems projection sqlitewrap osmelements osmdb ${GTKmm_LIBRARIES} ${Boost_SYSTEM_LIBRARY} ${Boost_FILESYSTEM_LIBRARY} ${Boost_SIGNALS_LIBRARY})
target_link_libraries(displayer displaylib pathfind)
add_dependencies(displayer display_to_show)
