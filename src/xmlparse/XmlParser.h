/*
 * XmlParser.h
 *
 *  Created on: Nov 12, 2011
 *      Author: martin
 */

#ifndef XMLPARSER_H_
#define XMLPARSER_H_

#include "osm_xml-pimpl.hxx"
#include "../elements/osmelements.h"
#include <glibmm/ustring.h>
#include <functional>

namespace osmxml
{

/**
 * \class XmlParser
 * Responsible for reading osm xml exports and converting them to osm::Element. Reads input and calls
 * registered callbacks as elements are encoutered.
 */
class XmlParser
{
public:
    XmlParser();
    virtual ~XmlParser();
    /**
     * Node callback
     */
    std::function<void (osm::Node const&)> node_handler;
    /**
     * Way callback
     */
    std::function<void (osm::Way const&)> way_handler;
    /**
     * Relation callback
     */
    std::function<void (osm::Relation const&)> relation_handler;
    /**
     * This gets called after every element - to notify that progress is being made
     */
    std::function<void ()> progress_handler;
    /**
     * Process xml file
     * @param filename name of the file
     */
    void parse_file(std::string const& filename);
    /**
     * Process xml in std::string
     * @param mem std::string containing xml to be processed
     */
    void parse_memory(std::string const& mem);
    /**
     * Process xml from stream
     * @param stream
     */
    void parse_stream(std::istream& stream);
private:
    bound_pimpl bound_p;
    member_type_pimpl mt_p;
    member_pimpl member_p;
    nd_pimpl nd_p;
    node_pimpl node_p;
    osm_pimpl osm_p;
    relation_pimpl rel_p;
    tag_pimpl tag_p;
    way_pimpl way_p;
    xml_schema::string_pimpl string_p;
    xml_schema::uri_pimpl uri_p;
    xml_schema::long_pimpl long_p;
    xml_schema::decimal_pimpl decimal_p;
    xml_schema::boolean_pimpl bool_p;
    xml_schema::date_time_pimpl date_time_p;
    xml_schema::int_pimpl int_p;
    xml_schema::document doc_p;
};

} /* namespace osmxml */
#endif /* XMLPARSER_H_ */
