add_library(xmlparse XmlParser.cpp NodePiece.cpp ParserPiece.cpp TaggablePiece.cpp TagPiece.cpp HandlerPiece.cpp UnknownPiece.cpp OsmPiece.cpp)

target_link_libraries(xmlparse ${LIBXMLPP_LIBRARIES} ${Boost_SIGNALS_LIBRARY})

