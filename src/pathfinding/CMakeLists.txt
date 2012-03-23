add_library(pathfind OrderedCostNodeSet.cpp PathFinder.cpp Route.cpp)
target_link_libraries(pathfind roads)
