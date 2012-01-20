add_library(wayred WayNodeFilter.cpp)

add_executable(wayreduction main.cc)

target_link_libraries(wayreduction wayred osmdb ${Boost_PROGRAM_OPTIONS_LIBRARY} ${Boost_SYSTEM_LIBRARY} ${Boost_FILESYSTEM_LIBRARY} ${Boost_REGEX_LIBRARY})

configure_file(ways.txt ${CMAKE_BINARY_DIR}/share/way.txt COPYONLY)