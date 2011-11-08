/*
 * WayPiece.h
 *
 *  Created on: Nov 7, 2011
 *      Author: martin
 */

#ifndef WAYPIECE_H_
#define WAYPIECE_H_

#include "ParserPiece.h"
#include "TaggablePiece.h"
#include "HandlerPiece.h"
#include "../elements/osmelements.h"

namespace osmxml
{

class WayPiece: public TaggablePiece
{
public:
    virtual ~WayPiece();
    WayPiece(HandlerPiece* parent, Glib::ustring name, xmlpp::SaxParser::AttributeList const& attrs);
    ParserPiece* handle_start_element(Glib::ustring const& name, xmlpp::SaxParser::AttributeList const& attrs);
    ParserPiece* handle_end_element(Glib::ustring const& name);
    void add_tag(Glib::ustring const& key, Glib::ustring const& value);
    void add_node(int64_t id);
private:
    HandlerPiece* parent;
    Glib::ustring name;
    osm::Way w;
};

} /* namespace osmxml */
#endif /* WAYPIECE_H_ */
