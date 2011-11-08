/*
 * XmlParser.cpp
 *
 *  Created on: Nov 7, 2011
 *      Author: martin
 */

#include "XmlParser.h"
#include "OsmPiece.h"

namespace osmxml
{

XmlParser::XmlParser():
    done(false),
    cur_piece(NULL)
{
}

XmlParser::~XmlParser()
{
    if (cur_piece != NULL)
        delete cur_piece;
}

boost::signal<void(const osm::Node& )> & XmlParser::node_signal()
{
    return node_sig;
}

boost::signal<void(const osm::Way& )> & XmlParser::way_signal()
{
    return way_sig;
}

boost::signal<void(const osm::Relation& )> & XmlParser::relation_signal()
{
    return relation_sig;
}

boost::signal<void(const Glib::ustring& )> & XmlParser::warn_signal()
{
    return warn_sig;
}

void XmlParser::on_warning(const Glib::ustring& msg)
{
    warn_sig(msg);
}

void XmlParser::on_error(const Glib::ustring& msg)
{
    throw std::exception(); //TODO
}

void XmlParser::on_fatal_error(const Glib::ustring& msg)
{
    throw std::exception(); //TODO
}

void XmlParser::on_start_element(const Glib::ustring& name, const AttributeList& attributes)
{
    if (!done && !cur_piece)
    {
        cur_piece = new OsmPiece(*this, name);
    }
    else
    {
        cur_piece = cur_piece->handle_start_element(name, attributes);
    }

}

void XmlParser::on_end_element(const Glib::ustring& name)
{
    cur_piece = cur_piece->handle_end_element(name);
}

} /* namespace osmxml */
