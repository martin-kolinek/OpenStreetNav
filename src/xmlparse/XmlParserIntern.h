/*
 * XmlParserIntern.h
 *
 *  Created on: Nov 7, 2011
 *      Author: martin
 */

#ifndef XMLPARSERINTERN_H_
#define XMLPARSERINTERN_H_

#include <libxml++/libxml++.h>
#include <boost/signals.hpp>
#include "../elements/osmelements.h"
#include "XmlParserException.h"
#include "SubParser.h"
#include "UnknownParser.h"


namespace osmxml
{

class XmlParserIntern : public xmlpp::SaxParser
{
public:
    XmlParserIntern();
    virtual ~XmlParserIntern();
    bool success();
    XmlParserException exception();
    std::function<void (osm::Node const&)> node_handler;
    std::function<void (osm::Way const&)> way_handler;
    std::function<void (osm::Relation const&)> relation_handler;
    std::function<void (const Glib::ustring&)> warn_handler;
    std::function<void ()> progress_handler;
protected:
    void on_start_element(const Glib::ustring& name, const AttributeList& attributes);
    void on_end_element(const Glib::ustring& name);
    void on_warning(const Glib::ustring& msg);
    void on_error(const Glib::ustring& msg);
    void on_fatal_error(const Glib::ustring& msg);
private:
    bool done, started;
    UnknownParser unk;
    SubParser<osm::Tag> tagpars;
    SubParser<osm::RelationMapping> memberpars;
    SubParser<int64_t> ndpars;
    SubParser<osm::Node, osm::Tag> nodepars;
    SubParser<osm::Way, osm::Tag, int64_t> waypars;
    SubParser<osm::Relation, osm::Tag, osm::RelationMapping> relpars;
    SubParser<int, osm::Node, osm::Way, osm::Relation, int> pars;
    XmlParserException exc;
    bool ok;
};

} /* namespace osmxml */
#endif /* XMLPARSERINTERN_H_ */
