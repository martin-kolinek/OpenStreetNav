/*
 * XmlParser.h
 *
 *  Created on: Nov 7, 2011
 *      Author: martin
 */

#ifndef XMLPARSER_H_
#define XMLPARSER_H_

#include <libxml++/libxml++.h>
#include <boost/signals.hpp>
#include "../elements/osmelements.h"
#include "ParserPiece.h"

namespace osmxml
{

class XmlParser : public xmlpp::SaxParser
{
public:
    XmlParser();
    virtual ~XmlParser();
    boost::signal<void (osm::Node const&)>& node_signal();
    boost::signal<void (osm::Way const&)>& way_signal();
    boost::signal<void (osm::Relation const&)>& relation_signal();
    boost::signal<void (const Glib::ustring&)>& warn_signal();
protected:
    void on_start_element(const Glib::ustring& name, const AttributeList& attributes);
    void on_end_element(const Glib::ustring& name);
    void on_warning(const Glib::ustring& msg);
    void on_error(const Glib::ustring& msg);
    void on_fatal_error(const Glib::ustring& msg);
private:
    boost::signal<void (osm::Node const&)> node_sig;
    boost::signal<void (osm::Way const&)> way_sig;
    boost::signal<void (osm::Relation const&)> relation_sig;
    boost::signal<void (const Glib::ustring&)> warn_sig;
    bool done;
    ParserPiece* cur_piece;
};

} /* namespace osmxml */
#endif /* XMLPARSER_H_ */
