add_executable(importer main.cc)
target_link_libraries(importer osmdb xmlparse ${Boost_PROGRAM_OPTIONS_LIBRARY} ${Boost_SYSTEM_LIBRARY} ${Boost_FILESYSTEM_LIBRARY})