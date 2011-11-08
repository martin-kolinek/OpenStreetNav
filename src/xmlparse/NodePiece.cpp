/*
 * NodePiece.cpp
 *
 *  Created on: Nov 7, 2011
 *      Author: martin
 */

#include "NodePiece.h"
#include "TagPiece.h"
#include "../util.h"

namespace osmxml
{

NodePiece::~NodePiece()
{
    if (parent != NULL)
        delete parent;
}

NodePiece::NodePiece(HandlerPiece* parent, Glib::ustring name, const xmlpp::SaxParser::AttributeList& attrs)
    : parent(parent),
      name(name),
      nd(0, 0, 0)
{
    for (auto it = attrs.begin(); it != attrs.end(); ++it)
    {
        auto nm = it->name;
        if (nm == "id")
        {
            nd.id = parse<int64_t>(it->value);
        }
        else if (nm == "lat")
        {
            nd.lat = parse<double>(it->value);
        }
        else if (nm == "lon")
        {
            nd.lon = parse<double>(it->value);
        }
    }

}

ParserPiece* NodePiece::handle_start_element(const Glib::ustring& name, const xmlpp::SaxParser::AttributeList& attrs)
{
    if (name == "tag")
    {
        return new TagPiece(this, name, attrs);
    }
    throw std::exception(); //TODO
}

ParserPiece* NodePiece::handle_end_element(const Glib::ustring& name)
{
    if (name != this->name)
        throw std::exception(); //TODO
    auto local = parent;
    parent = NULL;
    local->handle_node(nd);
    delete this;
    return local;
}

void NodePiece::add_tag(const Glib::ustring& key, const Glib::ustring& value)
{
    nd.tags.push_back(osm::Tag(key, value));
}

} /* namespace osmxml */
