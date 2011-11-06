add_library(osmdb OsmDatabase.cpp ElementInsertion.cpp)
target_link_libraries(osmdb sqlitewrap)