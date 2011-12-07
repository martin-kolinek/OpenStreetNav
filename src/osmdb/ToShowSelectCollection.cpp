/*
 * ToShowSelectCollection.cpp
 *
 *  Created on: Dec 7, 2011
 *      Author: martin
 */

#include "ToShowSelectCollection.h"
#include <boost/property_tree/ptree.hpp>
#include <boost/property_tree/xml_parser.hpp>
#include "../sqllib/sqllib.h"

namespace osmdb
{

ToShowSelectCollection::ToShowSelectCollection(const std::string& path_base, psql::Database& db, int min, int max)
{
    offset = min;
    for (int i = min; i <= max; ++i)
    {
        std::ostringstream ss;
        ss << path_base << i << ".xml";
        boost::property_tree::ptree entries;
        boost::property_tree::xml_parser::read_xml(ss.str(), entries, boost::property_tree::xml_parser::trim_whitespace);
        statements.push_back(sqllib::get_toshow_edges_select(entries, db));
    }
}

psql::Statement<psql::BindTypes<double, double, double, double> , psql::RetTypes<double, double, double, double, int64_t, double, double, double, double, int, int> >& ToShowSelectCollection::get_edges_for_zoom(int zoom)
{
    return statements[zoom-offset];
}

}


/* namespace osmdb */
