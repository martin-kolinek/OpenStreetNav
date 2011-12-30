/*
 * KeyValFilterTranslator.cpp
 *
 *  Created on: Dec 5, 2011
 *      Author: martin
 */

#include "KeyValFilterTranslator.h"

namespace sqllib
{

KeyValFilterTranslator::KeyValFilterTranslator(std::string const& cols, std::string const& tables, std::string const& where, std::string const& kvtable, std::vector<std::string> const& types):
    cols(cols),
    tables(tables),
    where_cond(where),
    kvtable(kvtable),
    types(types)
{
}

boost::property_tree::ptree KeyValFilterTranslator::translate(const boost::property_tree::ptree& input)
{
    boost::property_tree::ptree ret;
    ret.put("type", "union");
    boost::property_tree::ptree retchldrn;
    for (auto it = input.get_child("entries").begin(); it != input.get_child("entries").end(); ++it)
    {
        boost::property_tree::ptree const& entry = it->second;
        boost::property_tree::ptree chld;
        boost::property_tree::ptree add = entry.get_child("add");
        chld.put("type", "intersect");
        boost::property_tree::ptree chldrn;
        for (auto it2 = entry.get_child("elements").begin(); it2 != entry.get_child("elements").end(); ++it2)
        {
            boost::property_tree::ptree el;
            el.put("type", "simple");
            std::ostringstream s;
            s << "SELECT " << cols;
            unsigned int i = 0;
            for (auto it3 = add.begin(); it3 != add.end(); ++it3)
            {
                s << ", " << it3->second.data();
                if (i < types.size())
                    s << "::" << types[i];
                i++;
            }
            s << " FROM " << tables << " WHERE " << where_cond;
            s << " AND " << kvtable << ".Key=" << "'" << it2->second.get<std::string>("key") << "'";
            s << " AND " << kvtable << ".Value=" << "'" << it2->second.get<std::string>("value") << "'";
            el.put("query", s.str());
            chldrn.add_child("child", el);
        }
        chld.put_child("children", chldrn);
        retchldrn.add_child("child", chld);
    }
    ret.put_child("children", retchldrn);
    return ret;
}

} /* namespace sqllib */
