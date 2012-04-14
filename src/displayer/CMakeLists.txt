add_library(displaylib MapDrawingArea.cpp DisplayElement.cpp DisplayStyle.cpp LineDisplayStyle.cpp DisplayLine.cpp DisplayProvider.cpp EdgeHighlighter.cpp Descriptible.cpp DisplayStyleChanger.cpp ColorStyleChanger.cpp DescriptibleElement.cpp Thickener.cpp DescriptibleRoadEdge.cpp)
add_executable(displayer main.cc)
configure_file(display.glade ${CMAKE_BINARY_DIR}/share/display.glade COPYONLY)
SET(GLADE_PATH ${CMAKE_BINARY_DIR}/share/display.glade)

configure_file(${CMAKE_SOURCE_DIR}/src/displayer/config.h.in ${CMAKE_BINARY_DIR}/include/osmdisp_config.h)
configure_file(${CMAKE_SOURCE_DIR}/src/displayer/zoom_schemas.txt ${CMAKE_BINARY_DIR}/share/zoom_schemas.txt)
configure_file(${CMAKE_SOURCE_DIR}/src/displayer/road_zooms.txt ${CMAKE_BINARY_DIR}/share/road_zooms.txt)

target_link_libraries(displaylib geoelems projection osmelements osmdb ${GTKmm_LIBRARIES} ${Boost_SYSTEM_LIBRARY} ${Boost_FILESYSTEM_LIBRARY} ${Boost_SIGNALS_LIBRARY} ${Boost_REGEX_LIBRARY})
target_link_libraries(displayer displaylib pathfind)
add_dependencies(displayer display_to_show)
