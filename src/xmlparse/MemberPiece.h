/*
 * MemberPiece.h
 *
 *  Created on: Nov 7, 2011
 *      Author: martin
 */

#ifndef MEMBERNDPIECE_H_
#define MEMBERNDPIECE_H_

#include "ParserPiece.h"
#include "TaggablePiece.h"
#include "RelationPiece.h"

namespace osmxml
{

class MemberPiece : public ParserPiece
{
public:
    virtual ~MemberPiece();
    MemberPiece(RelationPiece* pc, Glib::ustring const& name, xmlpp::SaxParser::AttributeList const& attrs);
    ParserPiece* handle_start_element(Glib::ustring const&, xmlpp::SaxParser::AttributeList const&);
    ParserPiece* handle_end_element(const Glib::ustring& name);
private:
    RelationPiece* pc;
    Glib::ustring name;
};

} /* namespace osmxml */
#endif /* MEMBERPIECE_H_ */
