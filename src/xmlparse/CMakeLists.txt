add_library(xmlparse XmlParserIntern.cpp XmlParser.cpp SubParser.cpp UnknownParser.cpp XmlParserException.cpp)

target_link_libraries(xmlparse util ${LibXML++_LIBRARIES} ${Boost_SIGNALS_LIBRARY})

