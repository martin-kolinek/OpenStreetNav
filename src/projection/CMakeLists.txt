add_library(projection OrthoProjection.cpp MapProjection.cpp FlatPoint.cpp)

target_link_libraries(projection geoelems)