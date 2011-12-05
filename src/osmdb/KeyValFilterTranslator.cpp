/*
 * KeyValFilterTranslator.cpp
 *
 *  Created on: Dec 5, 2011
 *      Author: martin
 */

#include "KeyValFilterTranslator.h"

namespace osmdb
{

boost::property_tree::ptree osmdb::KeyValFilterTranslator::translate(const boost::property_tree::ptree& input)
{
    boost::property_tree::ptree ret;
    ret.put("type", "union");
    boost::property_tree::ptree retchldrn;
    for (auto it = input.get_child("entries").begin(); it != input.get_child("entries").end(); ++it)
    {
        boost::property_tree::ptree const& entry = it->second;
        boost::property_tree::ptree chld;
        int zoom = entry.get<int>("zoom");
        double r = entry.get<double>("red");
        double g = entry.get<double>("green");
        double b = entry.get<double>("blue");
        double th = entry.get<double>("thickness");
        chld.put("type", "intersect");
        boost::property_tree::ptree chldrn;
        for (auto it2 = entry.get_child("elements").begin(); it2 != entry.get_child("elements").end(); ++it2)
        {
            boost::property_tree::ptree el;
            el.put("type", "simple");
            std::ostringstream s;
            s << "SELECT " << cols;
            s << ", " << zoom;
            s << ", " << r;
            s << ", " << g;
            s << ", " << b;
            s << ", " << th;
            s << " FROM " << tables << " WHERE " << where_cond;
            s << " AND " << kvtable << ".Key=" << it->second.get<std::string>("key");
            s << " AND " << kvtable << ".Value=" << it->second.get<std::string>("value");
            el.put("query", s.str());
            chldrn.add_child("child", el);
        }
        chld.put_child("children", chldrn);
        retchldrn.add_child("child", chld);
    }
    ret.put_child("children", retchldrn);
    return ret;
}

} /* namespace osmdb */
