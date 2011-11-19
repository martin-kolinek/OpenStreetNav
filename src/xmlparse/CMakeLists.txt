add_library(xmlparse XmlParserIntern.cpp XmlParser.cpp SubParser.cpp UnknownParser.cpp XmlParserException.cpp)

target_link_libraries(xmlparse util ${LIBXMLPP_LIBRARIES} ${Boost_SIGNALS_LIBRARY})

