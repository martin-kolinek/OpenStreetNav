add_library(osmdb OsmDatabase.cpp ElementInsertion.cpp WrongDBException.cpp)
target_link_libraries(osmdb sqlitewrap)