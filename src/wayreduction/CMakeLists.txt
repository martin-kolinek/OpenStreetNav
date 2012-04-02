add_library(wayred WayNodeFilter.cpp)

add_executable(wayreduction main.cc)

target_link_libraries(wayreduction wayred osmdb ${Boost_PROGRAM_OPTIONS_LIBRARY} ${Boost_SYSTEM_LIBRARY} ${Boost_FILESYSTEM_LIBRARY} ${Boost_REGEX_LIBRARY})

configure_file(ways.txt ${CMAKE_BINARY_DIR}/share/way.txt COPYONLY)
configure_file(map1.txt ${CMAKE_BINARY_DIR}/share/map1.txt COPYONLY)
configure_file(map2.txt ${CMAKE_BINARY_DIR}/share/map2.txt COPYONLY)
configure_file(map3.txt ${CMAKE_BINARY_DIR}/share/map3.txt COPYONLY)
configure_file(map5.txt ${CMAKE_BINARY_DIR}/share/map5.txt COPYONLY)
configure_file(map6.txt ${CMAKE_BINARY_DIR}/share/map6.txt COPYONLY)
