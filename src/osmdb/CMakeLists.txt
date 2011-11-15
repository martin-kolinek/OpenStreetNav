add_library(osmdb OsmDatabase.cpp ElementInsertion.cpp WrongDBException.cpp DisplayDB.cpp)
target_link_libraries(osmdb sqlitewrap osmelements)
