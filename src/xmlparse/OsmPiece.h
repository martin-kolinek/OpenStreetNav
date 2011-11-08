/*
 * OsmPiece.h
 *
 *  Created on: Nov 8, 2011
 *      Author: martin
 */

#ifndef OSMPIECE_H_
#define OSMPIECE_H_

#include "XmlParser.h"
#include "HandlerPiece.h"

namespace osmxml
{

class OsmPiece: public osmxml::HandlerPiece
{
public:
    OsmPiece(XmlParser& parser, Glib::ustring const& name);
    virtual ~OsmPiece();
    void handle_node(osm::Node const& nd);
    void handle_way(osm::Way const& w);
    void handle_relation(osm::Relation const& r);
    ParserPiece* handle_start_element(Glib::ustring const& name, xmlpp::SaxParser::AttributeList const& attrs);
    ParserPiece* handle_end_element(const Glib::ustring& name);
private:
    XmlParser& parser;
    Glib::ustring name;
};

} /* namespace osmxml */
#endif /* OSMPIECE_H_ */
