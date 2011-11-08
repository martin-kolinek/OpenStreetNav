/*
 * RelationPiece.cpp
 *
 *  Created on: Nov 7, 2011
 *      Author: martin
 */

#include "RelationPiece.h"
#include "TagPiece.h"
#include "MemberPiece.h"
#include "../util.h"

namespace osmxml
{

RelationPiece::~RelationPiece()
{
    if (parent != NULL)
        delete parent;
}

RelationPiece::RelationPiece(HandlerPiece* parent, Glib::ustring name, const xmlpp::SaxParser::AttributeList& attrs)
    : parent(parent),
      name(name),
      r(0)
{
    for (auto it = attrs.begin(); it != attrs.end(); ++it)
    {
        auto nm = it->name;
        if (nm == "id")
        {
            r.id = parse<int64_t>(it->value);
        }
    }

}

ParserPiece* RelationPiece::handle_start_element(const Glib::ustring& name, const xmlpp::SaxParser::AttributeList& attrs)
{
    if (name == "tag")
    {
        return new TagPiece(this, name, attrs);
    }
    else if (name == "member")
    {
        return new MemberPiece(this, name, attrs);
    }
    throw std::exception(); //TODO
}

ParserPiece* RelationPiece::handle_end_element(const Glib::ustring& name)
{
    if (name != this->name)
        throw std::exception(); //TODO
    auto local = parent;
    parent = NULL;
    local->handle_relation(r);
    delete this;
    return local;
}

void RelationPiece::add_tag(const Glib::ustring& key, const Glib::ustring& value)
{
    r.tags.push_back(osm::Tag(key, value));
}

void RelationPiece::add_member(osm::RelationMapping const& mmbr)
{
    r.members.push_back(mmbr);
}

} /* namespace osmxml */
