/*
 * ParserPiece.h
 *
 *  Created on: Nov 7, 2011
 *      Author: martin
 */

#ifndef PARSERPIECE_H_
#define PARSERPIECE_H_

#include <libxml++/libxml++.h>

namespace osmxml
{

class ParserPiece
{
public:
    virtual ParserPiece* handle_start_element(Glib::ustring const& name, xmlpp::SaxParser::AttributeList const& attrs) = 0;
    virtual ParserPiece* handle_end_element(const Glib::ustring& name) = 0;
    virtual ~ParserPiece();
};

} /* namespace osmxml */
#endif /* PARSERPIECE_H_ */
