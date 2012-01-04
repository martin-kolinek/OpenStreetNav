add_library(xmlparse XmlParser.cpp UnknownParser.cpp XmlParserException.cpp 
    bound-pimpl.cxx
    member_type-pimpl.cxx
    member-pimpl.cxx
    nd-pimpl.cxx
    node-pimpl.cxx
    osm_xml-pimpl.cxx
    osm-pimpl.cxx
    relation-pimpl.cxx
    tag-pimpl.cxx
    way-pimpl.cxx
    bound-pskel.cxx
    member_type-pskel.cxx
    member-pskel.cxx
    nd-pskel.cxx
    node-pskel.cxx
    osm_xml-pskel.cxx
    osm-pskel.cxx
    relation-pskel.cxx
    tag-pskel.cxx
    way-pskel.cxx
    )

target_link_libraries(xmlparse util ${LibXML++_LIBRARIES} ${Boost_SIGNALS_LIBRARY})

