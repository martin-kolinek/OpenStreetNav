find_package(PkgConfig)

pkg_check_modules(LIBXMLPP REQUIRED libxml++-2.6)

link_directories(${LIBXMLPP_LIBRARY_DIRS} )

include_directories(${LIBXMLPP_INCLUDE_DIRS})

add_library(xmlparse XmlParser.cpp NodePiece.cpp ParserPiece.cpp TaggablePiece.cpp TagPiece.cpp HandlerPiece.cpp UnknownPiece.cpp OsmPiece.cpp)

target_link_libraries(xmlparse ${LIBXMLPP_LIBRARIES} ${Boost_SIGNALS_LIBRARY})

