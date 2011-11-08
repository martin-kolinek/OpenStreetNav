/*
 * MemberPiece.cpp
 *
 *  Created on: Nov 7, 2011
 *      Author: martin
 */

#include "MemberPiece.h"
#include <exception>
#include "../util.h"
#include "../elements/osmelements.h"

namespace osmxml
{

MemberPiece::~MemberPiece()
{
    if (pc != NULL)
        delete pc;
}

MemberPiece::MemberPiece(RelationPiece* pc, Glib::ustring const& name, xmlpp::SaxParser::AttributeList const& attrs):
    pc(pc),
    name(name)
{
    bool has_ref = false;
    bool has_type = false;
    bool has_role = false;
    int64_t ref;
    osm::ObjectType tp;
    std::string role;
    for (auto it = attrs.begin(); it != attrs.end(); ++it)
    {
        if (it->name == "ref")
        {
            if (has_ref)
                throw std::exception(); //TODO
            has_ref = true;
            ref = parse<int64_t>(it->value);
        }
        if (it->name == "type")
        {
            if (has_type)
                throw std::exception(); //TODO
            has_type = true;
            auto v = it->value;
            if (v == "node")
                tp = osm::ObjectType::Node;
            else if (v == "way")
                tp = osm::ObjectType::Way;
            else if (v == "relation")
                tp = osm::ObjectType::Relation;
        }
        if (it->name == "role")
        {
            if (has_role)
                throw std::exception(); //TODO
            has_role = true;
            role = it->value;
        }
    }
    if (has_ref && has_type && has_role)
        pc->add_member(osm::RelationMapping(role, tp, ref));
    else
        throw std::exception(); //TODO
}

ParserPiece* MemberPiece::handle_start_element(const Glib::ustring&, const xmlpp::SaxParser::AttributeList&)
{
    throw std::exception(); //TODO
}

ParserPiece* MemberPiece::handle_end_element(const Glib::ustring& name)
{
    if (name != this->name)
        throw std::exception(); //TODO
    auto local = pc;
    pc = NULL;
    delete this;
    return local;
}

} /* namespace osmxml */
