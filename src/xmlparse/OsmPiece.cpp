/*
 * OsmPiece.cpp
 *
 *  Created on: Nov 8, 2011
 *      Author: martin
 */

#include "OsmPiece.h"
#include "NodePiece.h"
#include "WayPiece.h"
#include "RelationPiece.h"
#include "UnknownPiece.h"

namespace osmxml
{

OsmPiece::OsmPiece(XmlParser& parser, Glib::ustring const& name):
    parser(parser),
    name(name)
{
}

OsmPiece::~OsmPiece()
{
}

void OsmPiece::handle_node(const osm::Node& nd)
{
    parser.node_signal()(nd);
}

void OsmPiece::handle_way(const osm::Way& w)
{
    parser.way_signal()(w);
}

void OsmPiece::handle_relation(const osm::Relation& r)
{
    parser.relation_signal()(r);
}

ParserPiece* OsmPiece::handle_start_element(const Glib::ustring& name, const xmlpp::SaxParser::AttributeList& attrs)
{
    if (name == "node")
    {
        return new NodePiece(this, name, attrs);
    }
    else if (name == "way")
    {
        return new WayPiece(this, name, attrs);
    }
    else if (name == "relation")
    {
        return new RelationPiece(this, name, attrs);
    }
    else
    {
        return new UnknownPiece(this, name);
    }
}

ParserPiece* OsmPiece::handle_end_element(const Glib::ustring& name)
{
    if (name != this->name)
    {
        throw std::exception(); //TODO
    }
    else
    {
        delete this;
        return NULL;
    }
}

} /* namespace osmxml */
