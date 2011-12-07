add_library(osmdb OsmDatabase.cpp WrongDBException.cpp ElementInsertion.cpp DisplayDB.cpp ToShowSelectCollection.cpp)
target_link_libraries(osmdb psql sqllib osmelements)
