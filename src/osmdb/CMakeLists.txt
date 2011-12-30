add_library(osmdb OsmDatabase.cpp WrongDBException.cpp ElementInsertion.cpp DisplayDB.cpp ToShowSelectCollection.cpp)
target_link_libraries(osmdb displaylib psql sqllib osmelements)
