/*
 * XmlParser.cpp
 *
 *  Created on: Nov 7, 2011
 *      Author: martin
 */

#include "XmlParser.h"
#include "XmlParserException.h"
#include "../util.h"

namespace osmxml
{

std::vector<std::string> get_strings(std::vector<std::string> const& interesting, xmlpp::SaxParser::AttributeList const& list)
{
	std::vector<std::string> ret(interesting.size());
	std::vector<bool> got(interesting.size(), false);
	for(unsigned int i=0; i<list.size(); ++i)
	{
		int intr_index=-1;
		for(unsigned int j=0; j<interesting.size(); ++j)
		{
			if(list[i].name==interesting[j])
			{
				intr_index=j;
				break;
			}
		}
		if(intr_index>=0)
		{
			got[intr_index]=true;
			ret[intr_index]=list[i].value;
		}
	}
	for(unsigned int i=0; i<got.size(); ++i)
	{
		if(!got[i])
		{
			throw XmlParserException("Could not find attribute "+interesting[i]);
		}
	}
	return ret;
}

void parse_tag_attrs(osm::Tag& tg, xmlpp::SaxParser::AttributeList const& list)
{
	auto vals=get_strings({"k", "v"}, list);
	tg.key=vals[0];
	tg.value=vals[1];
}

void parse_node_attrs(osm::Node& nd, xmlpp::SaxParser::AttributeList const& list)
{
	auto vals=get_strings({"id", "lat", "lon"}, list);
	nd.id=parse<int64_t>(vals[0]);
	nd.lat=parse<double>(vals[1]);
	nd.lon=parse<double>(vals[2]);
}

void parse_member_attrs(osm::RelationMapping& rm, xmlpp::SaxParser::AttributeList const& list)
{
	auto vals=get_strings({"type", "ref", "role"}, list);
	rm.id=parse<int64_t>(vals[1]);
	if(vals[0]=="node")
		rm.type=osm::ObjectType::Node;
	if(vals[0]=="way")
		rm.type=osm::ObjectType::Way;
	if(vals[0]=="relation")
		rm.type=osm::ObjectType::Relation;
	rm.role=vals[2];
}

void parse_nd_attrs(int64_t& id, xmlpp::SaxParser::AttributeList const& list)
{
	auto vals=get_strings({"ref"}, list);
	id=parse<int64_t>(vals[0]);
}

void parse_way_attrs(osm::Way& w, xmlpp::SaxParser::AttributeList const& list)
{
	auto vals=get_strings({"id"}, list);
	w.id=parse<int64_t>(vals[0]);
}

void parse_rel_attrs(osm::Relation& r, xmlpp::SaxParser::AttributeList const& list)
{
	auto vals=get_strings({"id"}, list);
	r.id=parse<int64_t>(vals[0]);
}

XmlParser::XmlParser():
    done(false),
    started(false),
    tagpars(
    		parse_tag_attrs
    ),
    memberpars(
    		parse_member_attrs
    ),
    ndpars(
    		parse_nd_attrs
    ),
    nodepars(
    		parse_node_attrs,
    		pa<osm::Node, osm::Tag>("tag",
    				tagpars,
    				[](osm::Node& nd, osm::Tag const& tg){nd.tags.push_back(tg);}
    		)
    ),
    waypars(
    		parse_way_attrs,
    		pa<osm::Way, osm::Tag>("tag",
    				tagpars,
    				[](osm::Way& w, osm::Tag const& tg){w.tags.push_back(tg);}
    		),
    		pa<osm::Way, int64_t>("nd",
    				ndpars,
    				[](osm::Way& w, int64_t const& nd){w.nodes.push_back(nd);}
    		)
    ),
    relpars(
    		parse_rel_attrs,
    		pa<osm::Relation, osm::Tag>("tag", tagpars,
    				[](osm::Relation& r,
    						osm::Tag const& tg){r.tags.push_back(tg);}
    		),
    		pa<osm::Relation, osm::RelationMapping>("member",
    				memberpars,
    				[](osm::Relation& r, osm::RelationMapping const& mem){r.members.push_back(mem);}
    		)
    ),
    pars(
    		[](int&, xmlpp::SaxParser::AttributeList const&){},
    		pa<int, osm::Node>("node", nodepars, [&](int&, osm::Node const& nd){node_sig(nd);}),
    		pa<int, osm::Way>("way", waypars, [&](int&, osm::Way const& w){way_sig(w);}),
    		pa<int, osm::Relation>("relation", relpars, [&](int&, osm::Relation const& r){relation_sig(r);}),
    		pa<int, int>("", unk, [](int&, int const&){})
    )
{
}

XmlParser::~XmlParser()
{
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

boost::signal<void()> & XmlParser::progress_signal()
{
    return progress_sig;
}

void XmlParser::on_warning(const Glib::ustring& msg)
{
    warn_sig(msg);
}

void XmlParser::on_error(const Glib::ustring& msg)
{
    throw XmlParserException("libxml report: "+msg);
}

void XmlParser::on_fatal_error(const Glib::ustring& msg)
{
	throw XmlParserException("libxml report: "+msg);
}

void XmlParser::on_start_element(const Glib::ustring& name, const AttributeList& attributes)
{
    if (done)
    	throw XmlParserException("start element after being done");
    if(!started)
    {
    	if(name!="osm")
    		throw XmlParserException("xml not starting witm osm element");
    	started=true;
    	pars.reset(attributes);
    }
    else
    	pars.start(name, attributes);


}

void XmlParser::on_end_element(const Glib::ustring& name)
{
	if(pars.end(name))
		done=true;
}

} /* namespace osmxml */
