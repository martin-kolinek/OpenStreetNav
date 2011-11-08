/*
 * NodePiece.h
 *
 *  Created on: Nov 7, 2011
 *      Author: martin
 */

#ifndef NODEPIECE_H_
#define NODEPIECE_H_

#include "ParserPiece.h"
#include "TaggablePiece.h"
#include "HandlerPiece.h"
#include "../elements/osmelements.h"

namespace osmxml
{

class NodePiece: public TaggablePiece
{
public:
    virtual ~NodePiece();
    NodePiece(HandlerPiece* parent, Glib::ustring name, xmlpp::SaxParser::AttributeList const& attrs);
    ParserPiece* handle_start_element(Glib::ustring const& name, xmlpp::SaxParser::AttributeList const& attrs);
    ParserPiece* handle_end_element(Glib::ustring const& name);
    void add_tag(Glib::ustring const& key, Glib::ustring const& value);
private:
    HandlerPiece* parent;
    Glib::ustring name;
    osm::Node nd;
};

} /* namespace osmxml */
#endif /* NODEPIECE_H_ */
