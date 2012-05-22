add_library(pathfind OrderedCostNodeSet.cpp PathFinder.cpp Route.cpp AStarPathFinding.cpp PathFindingAlgorithm.cpp PathFindAlgorithmFactory.cpp)
target_link_libraries(pathfind roads)
