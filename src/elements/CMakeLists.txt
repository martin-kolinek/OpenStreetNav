add_library(osmelements Node.cpp Relation.cpp Relation.cpp Way.cpp Edge.cpp Element.cpp CompByID.cpp)

target_link_libraries(osmelements osmdb)