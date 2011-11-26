add_library(wkb WKBInterface.cpp)
target_link_libraries(wkb ${GEOS_LIBRARY} geoelems)