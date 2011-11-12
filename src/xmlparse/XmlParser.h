/*
 * XmlParser.h
 *
 *  Created on: Nov 12, 2011
 *      Author: martin
 */

#ifndef XMLPARSER_H_
#define XMLPARSER_H_

#include "XmlParserIntern.h"
#include "../elements/osmelements.h"
#include <glibmm/ustring.h>
#include <functional>

namespace osmxml
{

class XmlParser
{
public:
    XmlParser();
    virtual ~XmlParser();
    std::function<void (osm::Node const&)> node_handler;
    std::function<void (osm::Way const&)> way_handler;
    std::function<void (osm::Relation const&)> relation_handler;
    std::function<void (const Glib::ustring&)> warn_handler;
    std::function<void ()> progress_handler;
    void parse_file(std::string const& filename);
    void parse_memory(std::string const& mem);
};

} /* namespace osmxml */
#endif /* XMLPARSER_H_ */
