add_library(osmelements Node.cpp Relation.cpp Relation.cpp Way.cpp Edge.cpp Element.cpp CompByID.cpp WayRegion.cpp EdgeBase.cpp NodeEdge.cpp SeqEdge.cpp)

target_link_libraries(osmelements osmdb displaylib)
