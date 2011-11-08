/*
 * WayNdPiece.cpp
 *
 *  Created on: Nov 7, 2011
 *      Author: martin
 */

#include "WayNdPiece.h"
#include <exception>
#include "../util.h"

namespace osmxml
{

WayNdPiece::~WayNdPiece()
{
    if (pc != NULL)
        delete pc;
}

WayNdPiece::WayNdPiece(WayPiece* pc, Glib::ustring const& name, xmlpp::SaxParser::AttributeList const& attrs):
    pc(pc),
    name(name)
{
    bool has_ref = false;
    int64_t ref;
    for (auto it = attrs.begin(); it != attrs.end(); ++it)
    {
        if (it->name == "ref")
        {
            if (has_ref)
                throw std::exception(); //TODO
            has_ref = true;
            ref = parse<int64_t>(it->value);
        }
    }
    if (has_ref)
        pc->add_node(ref);
    else
        throw std::exception(); //TODO
}

ParserPiece* WayNdPiece::handle_start_element(const Glib::ustring&, const xmlpp::SaxParser::AttributeList&)
{
    throw std::exception(); //TODO
}

ParserPiece* WayNdPiece::handle_end_element(const Glib::ustring& name)
{
    if (name != this->name)
        throw std::exception(); //TODO
    auto local = pc;
    pc = NULL;
    delete this;
    return local;
}

} /* namespace osmxml */
