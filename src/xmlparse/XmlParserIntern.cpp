/*
 * XmlParser.cpp
 *
 *  Created on: Nov 7, 2011
 *      Author: martin
 */

#include "XmlParserIntern.h"
#include "../util.h"
#include <iostream>

namespace osmxml
{

std::vector<const Glib::ustring*> get_strings(const std::vector<std::string> & interesting, const xmlpp::SaxParser::AttributeList& list)
{
    std::vector<const Glib::ustring*> ret(interesting.size());
    unsigned int intr = 0;
    for (unsigned int i = 0; i < list.size(); ++i)
    {
        if (list[i].name == interesting[intr])
        {
            ret[intr++] = &list[i].value;
            if (intr >= interesting.size())
                break;

        }
    }

    if (intr < interesting.size())
        throw XmlParserException("Error parsing node attributes (missing attributes)");

    return ret;
}
void parse_tag_attrs(osm::Tag& tg, const xmlpp::SaxParser::AttributeList& list)
{
    auto vals = get_strings( {"k", "v"}, list);
    tg.key = *vals[0];
    tg.value = *vals[1];
}

void parse_node_attrs(osm::Node& nd, const xmlpp::SaxParser::AttributeList& list)
{
    auto vals = get_strings( {"id", "lat", "lon"}, list);
    nd.tags.clear();
    nd.id = parse<int64_t>(*vals[0]);
    nd.lat = parse<double>(*vals[1]);
    nd.lon = parse<double>(*vals[2]);
}

void parse_member_attrs(osm::RelationMapping& rm, const xmlpp::SaxParser::AttributeList& list)
{
    auto vals = get_strings( {"type", "ref", "role"}, list);
    rm.id = parse<int64_t>(*vals[1]);
    if (*vals[0] == "node")
        rm.type = osm::ObjectType::Node;

    if (*vals[0] == "way")
        rm.type = osm::ObjectType::Way;

    if (*vals[0] == "relation")
        rm.type = osm::ObjectType::Relation;

    rm.role = *vals[2];
}
void parse_nd_attrs(int64_t& id, const xmlpp::SaxParser::AttributeList& list)
{
    auto vals = get_strings( {"ref"}, list);
    id = parse<int64_t>(*vals[0]);
}

void parse_way_attrs(osm::Way& w, const xmlpp::SaxParser::AttributeList& list)
{
    auto vals = get_strings( {"id"}, list);
    w.tags.clear();
    w.nodes.clear();
    w.id = parse<int64_t>(*vals[0]);
}

void parse_rel_attrs(osm::Relation& r, const xmlpp::SaxParser::AttributeList& list)
{
    auto vals = get_strings( {"id"}, list);
    r.tags.clear();
    r.members.clear();
    r.id = parse<int64_t>(*vals[0]);
}

XmlParserIntern::XmlParserIntern():
    done(false), started(false), tagpars(parse_tag_attrs), memberpars(parse_member_attrs), ndpars(parse_nd_attrs), nodepars(parse_node_attrs, pa<osm::Node, osm::Tag>("tag", tagpars, [](osm::Node& nd, const osm::Tag& tg)
{
    nd.tags.push_back(tg);
}
                                                                                                                                                                 )), waypars(parse_way_attrs, pa<osm::Way, osm::Tag>("tag", tagpars, [](osm::Way& w, const osm::Tag& tg)
{
    w.tags.push_back(tg);
}
                                                                                                                                                                                                                    ), pa<osm::Way, int64_t>("nd", ndpars, [](osm::Way& w, const int64_t& nd)
{
    w.nodes.push_back(nd);
}
                                                                                                                                                                                                                                            )), relpars(parse_rel_attrs, pa<osm::Relation, osm::Tag>("tag", tagpars, [](osm::Relation& r, const osm::Tag& tg)
{
    r.tags.push_back(tg);
}
                                                                                                                                                                                                                                                                                                    ), pa<osm::Relation, osm::RelationMapping>("member", memberpars, [](osm::Relation& r, const osm::RelationMapping& mem)
{
    r.members.push_back(mem);
}
                                                                                                                                                                                                                                                                                                                                              )), pars([](int& , const xmlpp::SaxParser::AttributeList& )
{
}
, pa<int, osm::Node>("node", nodepars, [&](int& , const osm::Node& nd)
{
    node_handler(nd);
}
                    ), pa<int, osm::Way>("way", waypars, [&](int& , const osm::Way& w)
{
    way_handler(w);
}
                                        ), pa<int, osm::Relation>("relation", relpars, [&](int& , const osm::Relation& r)
{
    relation_handler(r);
}
                                                                 ), pa<int, int>("", unk, [](int& , const int& )
{
}
                                                                                )), exc(""), ok(true)
{
}

XmlParserIntern::~XmlParserIntern()
{
}

void XmlParserIntern::on_warning(const Glib::ustring& msg)
{
    warn_handler(msg);
}

void XmlParserIntern::on_error(const Glib::ustring& msg)
{
    exc = XmlParserException("libxml report: " + msg);
    ok = false;
}

bool XmlParserIntern::success()
{
    return ok;
}

XmlParserException XmlParserIntern::exception()
{
    return exc;
}

void XmlParserIntern::on_fatal_error(const Glib::ustring& msg)
{
    exc = XmlParserException("libxml report: " + msg);
    ok = false;
}

void XmlParserIntern::on_start_element(const Glib::ustring& name, const AttributeList& attributes)
{
    if (!ok)
        return;
    try
    {
        if (done)
            throw XmlParserException("start element after being done");
        progress_handler();
        if (!started)
        {
            if (name != "osm")
                throw XmlParserException("xml not starting witm osm element");
            started = true;
            pars.reset(attributes);
        }
        else
            pars.start(name, attributes);
    }
    catch (XmlParserException& ex)
    {
        exc = ex;
        ok = false;
    }

}

void XmlParserIntern::on_end_element(const Glib::ustring& name)
{
    if (!ok)
        return;
    try
    {
        if (pars.end(name))
            done = true;
    }
    catch (XmlParserException& ex)
    {
        exc = ex;
        ok = false;
    }
}

} /* namespace osmxml */
