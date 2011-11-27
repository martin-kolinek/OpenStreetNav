add_executable(displayer main.cc MapDrawingArea.cpp)
configure_file(display.glade ${CMAKE_BINARY_DIR}/bin/display.glade COPYONLY)
target_link_libraries(displayer geoelems projection sqlitewrap osmelements osmdb ${GTKMM_LIBRARIES} ${Boost_SYSTEM_LIBRARY} ${Boost_FILESYSTEM_LIBRARY} ${Boost_SIGNALS_LIBRARY})