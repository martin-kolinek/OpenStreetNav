add_library(xmlparse XmlParser.cpp SubParser.cpp UnknownParser.cpp XmlParserException.cpp)

target_link_libraries(xmlparse ${LIBXMLPP_LIBRARIES} ${Boost_SIGNALS_LIBRARY})

