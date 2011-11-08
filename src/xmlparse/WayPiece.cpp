/*
 * WayPiece.cpp
 *
 *  Created on: Nov 7, 2011
 *      Author: martin
 */

#include "WayPiece.h"
#include "TagPiece.h"
#include "WayNdPiece.h"
#include "../util.h"

namespace osmxml
{

WayPiece::~WayPiece()
{
    if (parent != NULL)
        delete parent;
}

WayPiece::WayPiece(HandlerPiece* parent, Glib::ustring name, const xmlpp::SaxParser::AttributeList& attrs)
    : parent(parent),
      name(name),
      w(0)
{
    for (auto it = attrs.begin(); it != attrs.end(); ++it)
    {
        auto nm = it->name;
        if (nm == "id")
        {
            w.id = parse<int64_t>(it->value);
        }
    }

}

ParserPiece* WayPiece::handle_start_element(const Glib::ustring& name, const xmlpp::SaxParser::AttributeList& attrs)
{
    if (name == "tag")
    {
        return new TagPiece(this, name, attrs);
    }
    else if (name == "nd")
    {
        return new WayNdPiece(this, name, attrs);
    }
    throw std::exception(); //TODO
}

ParserPiece* WayPiece::handle_end_element(const Glib::ustring& name)
{
    if (name != this->name)
        throw std::exception(); //TODO
    auto local = parent;
    parent = NULL;
    local->handle_way(w);
    delete this;
    return local;
}

void WayPiece::add_tag(const Glib::ustring& key, const Glib::ustring& value)
{
    w.tags.push_back(osm::Tag(key, value));
}

void WayPiece::add_node(int64_t id)
{
    w.nodes.push_back(id);
}

} /* namespace osmxml */
