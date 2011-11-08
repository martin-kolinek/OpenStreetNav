/*
 * UnknownPiece.h
 *
 *  Created on: Nov 8, 2011
 *      Author: martin
 */

#ifndef UNKNOWNPIECE_H_
#define UNKNOWNPIECE_H_

#include "ParserPiece.h"

namespace osmxml
{

class UnknownPiece : public ParserPiece
{
public:
    UnknownPiece(ParserPiece* parent, Glib::ustring const& name);
    ParserPiece* handle_start_element(Glib::ustring const& name, xmlpp::SaxParser::AttributeList const& attrs);
    ParserPiece* handle_end_element(const Glib::ustring& name);
    virtual ~UnknownPiece();
private:
    ParserPiece* parent;
    Glib::ustring name;
};

} /* namespace osmxml */
#endif /* UNKNOWNPIECE_H_ */
