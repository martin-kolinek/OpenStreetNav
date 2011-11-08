/*
 * TagPiece.cpp
 *
 *  Created on: Nov 7, 2011
 *      Author: martin
 */

#include "TagPiece.h"
#include <exception>

namespace osmxml
{

TagPiece::~TagPiece()
{
    if (pc != NULL)
        delete pc;
}

TagPiece::TagPiece(TaggablePiece* pc, Glib::ustring const& name, xmlpp::SaxParser::AttributeList const& attrs):
    pc(pc),
    name(name)
{
    bool has_key = false;
    bool has_val = false;
    std::string key;
    std::string val;
    for (auto it = attrs.begin(); it != attrs.end(); ++it)
    {
        if (it->name == "k")
        {
            if (has_key)
                throw std::exception(); //TODO
            has_key = true;
            key = it->value;
        }
        if (it->name == "v")
        {
            if (has_val)
                throw std::exception(); //TODO
            has_val = true;
            val = it->value;
        }
    }
    if (has_key && has_val)
        pc->add_tag(key, val);
    else
        throw std::exception(); //TODO
}

ParserPiece* TagPiece::handle_start_element(const Glib::ustring&, const xmlpp::SaxParser::AttributeList&)
{
    throw std::exception(); //TODO
}

ParserPiece* TagPiece::handle_end_element(const Glib::ustring& name)
{
    if (name != this->name)
        throw std::exception(); //TODO
    auto local = pc;
    pc = NULL;
    delete this;
    return local;
}

} /* namespace osmxml */
