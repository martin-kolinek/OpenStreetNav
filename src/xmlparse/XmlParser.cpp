/*
 * XmlParser.cpp
 *
 *  Created on: Nov 12, 2011
 *      Author: martin
 */

#include "XmlParser.h"

namespace osmxml
{


void empty_nd_hndl(const osm::Node& )
{
}

void empty_r_hndl(const osm::Relation& )
{
}

void empty_w_hndl(const osm::Way& )
{
}

void empty_prog_hndl()
{
}

void empty_msg_hndl(const std::string& )
{
}

XmlParser::XmlParser():
    node_handler(empty_nd_hndl),
    way_handler(empty_w_hndl),
    relation_handler(empty_r_hndl),
    warn_handler(empty_msg_hndl),
    progress_handler(empty_prog_hndl)
{
}

XmlParser::~XmlParser()
{
}

void XmlParser::parse_file(const std::string& filename)
{
    XmlParserIntern p;
    p.node_handler = node_handler;
    p.warn_handler = warn_handler;
    p.progress_handler = progress_handler;
    p.way_handler = way_handler;
    p.relation_handler = relation_handler;

    try
    {
        p.parse_file(filename);
    }
    catch (...)
    {
        if (!p.success())
        {
            throw p.exception();
        }
        else
            throw;
    }
    if (!p.success())
        throw p.exception();

}

void XmlParser::parse_memory(const std::string& mem)
{
    XmlParserIntern p;
    p.node_handler = node_handler;
    p.warn_handler = warn_handler;
    p.progress_handler = progress_handler;
    p.way_handler = way_handler;
    p.relation_handler = relation_handler;

    try
    {
        p.parse_memory(mem);
    }
    catch (...)
    {
        if (!p.success())
        {
            throw p.exception();
        }
        else
            throw;
    }
    if (!p.success())
        throw p.exception();
}

} /* namespace osmxml */
