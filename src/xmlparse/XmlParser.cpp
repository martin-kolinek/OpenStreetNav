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
    progress_handler(empty_prog_hndl),
    doc_p(osm_p, "osm")
{
    bound_p.parsers(string_p, uri_p);
    member_p.parsers(mt_p, long_p, string_p);
    nd_p.parsers(long_p);
    tag_p.parsers(string_p, string_p);
    node_p.parsers(tag_p, long_p, string_p, date_time_p, int_p, int_p, bool_p, decimal_p, decimal_p, string_p);
    rel_p.parsers(tag_p, member_p, long_p, string_p, date_time_p, int_p, int_p, bool_p);
    way_p.parsers(tag_p, nd_p, long_p, string_p, date_time_p, int_p, int_p, bool_p);
    osm_p.parsers(bound_p, node_p, way_p, rel_p, string_p, string_p);
    osm_p.node_handler = [&](osm::Node const & n)
    {
        this->node_handler(n);
    };
    osm_p.way_handler = [&](osm::Way const & w)
    {
        this->way_handler(w);
    };
    osm_p.relation_handler = [&](osm::Relation const & r)
    {
        this->relation_handler(r);
    };
}

XmlParser::~XmlParser()
{
}

void XmlParser::parse_file(const std::string& filename)
{
    osm_p.pre();
    doc_p.parse(filename);
    osm_p.post_osm();
}

void XmlParser::parse_memory(const std::string& mem)
{
    std::istringstream str(mem);
    parse_stream(str);
}

void XmlParser::parse_stream(std::istream& stream)
{
    osm_p.pre();
    doc_p.parse(stream);
    osm_p.post_osm();
}

} /* namespace osmxml */
