add_library(osmdb OsmDatabase.cpp WrongDBException.cpp ElementInsertion.cpp DisplayDB.cpp ToShowSelectCollection.cpp PropertiesSelection.cpp PropertiesSelectionException.cpp ElementImporter.cpp ElementCopy.cpp ImportTableProcessor.cpp WayLister.cpp AllWayLister.cpp RoadNetworkCreator.cpp RoadEdgeCopy.cpp EdgeCreator.cpp)
target_link_libraries(osmdb displaylib psql sqllib osmelements)
