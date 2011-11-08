/*
 * WayNdPiece.h
 *
 *  Created on: Nov 7, 2011
 *      Author: martin
 */

#ifndef WAYNDPIECE_H_
#define WAYNDPIECE_H_

#include "ParserPiece.h"
#include "TaggablePiece.h"
#include "WayPiece.h"

namespace osmxml
{

class WayNdPiece: public ParserPiece
{
public:
    virtual ~WayNdPiece();
    WayNdPiece(WayPiece* pc, Glib::ustring const& name, xmlpp::SaxParser::AttributeList const& attrs);
    ParserPiece* handle_start_element(Glib::ustring const&, xmlpp::SaxParser::AttributeList const&);
    ParserPiece* handle_end_element(const Glib::ustring& name);
private:
    WayPiece* pc;
    Glib::ustring name;
};

} /* namespace osmxml */
#endif /* WAYNDPIECE_H_ */
