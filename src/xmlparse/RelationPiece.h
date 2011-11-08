/*
 * RelationPiece.h
 *
 *  Created on: Nov 7, 2011
 *      Author: martin
 */

#ifndef RELATIONPIECE_H_
#define RELATIONPIECE_H_

#include "ParserPiece.h"
#include "TaggablePiece.h"
#include "HandlerPiece.h"
#include "../elements/osmelements.h"

namespace osmxml
{

class RelationPiece : public TaggablePiece
{
public:
    virtual ~RelationPiece();
    RelationPiece(HandlerPiece* parent, Glib::ustring name, xmlpp::SaxParser::AttributeList const& attrs);
    ParserPiece* handle_start_element(Glib::ustring const& name, xmlpp::SaxParser::AttributeList const& attrs);
    ParserPiece* handle_end_element(Glib::ustring const& name);
    void add_tag(Glib::ustring const& key, Glib::ustring const& value);
    void add_member(osm::RelationMapping const& mmbr);
private:
    HandlerPiece* parent;
    Glib::ustring name;
    osm::Relation r;
};

} /* namespace osmxml */
#endif /* RELATIONPIECE_H_ */
