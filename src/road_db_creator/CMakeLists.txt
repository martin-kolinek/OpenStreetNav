add_executable(roaddbcreate main.cc)
target_link_libraries(roaddbcreate osmdb cost ${Boost_PROGRAM_OPTIONS_LIBRARY} ${Boost_SYSTEM_LIBRARY} ${Boost_FILESYSTEM_LIBRARY} ${Boost_REGEX_LIBRARY})