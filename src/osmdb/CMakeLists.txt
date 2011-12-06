add_library(osmdb OsmDatabase.cpp WrongDBException.cpp ElementInsertion.cpp DisplayDB.cpp)
target_link_libraries(osmdb psql sqllib osmelements)
