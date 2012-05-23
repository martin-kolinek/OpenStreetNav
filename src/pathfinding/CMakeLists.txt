add_library(pathfind OrderedCostNodeSet.cpp PathFinder.cpp Route.cpp AStarPathFinding.cpp PathFindingAlgorithm.cpp PathFindAlgorithmFactory.cpp AreaFinder.cpp AreaAlgorithm.cpp AStarAreaFinding.cpp)
target_link_libraries(pathfind roads)
