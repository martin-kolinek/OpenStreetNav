/*
 * UnknownPiece.cpp
 *
 *  Created on: Nov 8, 2011
 *      Author: martin
 */

#include "UnknownPiece.h"

namespace osmxml
{

UnknownPiece::UnknownPiece(ParserPiece* parent, Glib::ustring const& name):
    parent(parent),
    name(name)
{
}

ParserPiece* UnknownPiece::handle_start_element(const Glib::ustring& name, const xmlpp::SaxParser::AttributeList&)
{
    return new UnknownPiece(this, name);
}

ParserPiece* UnknownPiece::handle_end_element(const Glib::ustring& name)
{
    if (name != this->name)
        throw std::exception(); //TODO
    auto local = parent;
    parent = NULL;
    delete this;
    return local;
}

UnknownPiece::~UnknownPiece()
{
    if (parent != NULL)
        delete parent;
}

} /* namespace osmxml */
