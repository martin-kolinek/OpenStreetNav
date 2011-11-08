/*
 * TagPiece.h
 *
 *  Created on: Nov 7, 2011
 *      Author: martin
 */

#ifndef TAGPIECE_H_
#define TAGPIECE_H_

#include "ParserPiece.h"
#include "TaggablePiece.h"

namespace osmxml
{

class TagPiece: public ParserPiece
{
public:
    virtual ~TagPiece();
    TagPiece(TaggablePiece* pc, Glib::ustring const& name, xmlpp::SaxParser::AttributeList const& attrs);
    ParserPiece* handle_start_element(Glib::ustring const&, xmlpp::SaxParser::AttributeList const&);
    ParserPiece* handle_end_element(const Glib::ustring& name);
private:
    TaggablePiece* pc;
    Glib::ustring name;
};

} /* namespace osmxml */
#endif /* TAGPIECE_H_ */
