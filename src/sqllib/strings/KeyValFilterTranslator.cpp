/*
 * KeyValFilterTranslator.cpp
 *
 *  Created on: Dec 5, 2011
 *      Author: martin
 */

#include "KeyValFilterTranslator.h"

namespace sqllib
{

KeyValFilterTranslator::KeyValFilterTranslator(std::string const& cols, std::string const& tables, std::string const& where, std::string const& kvtable, std::vector<std::string> const& types, std::string const& order_by):
    cols(cols),
    tables(tables),
    where_cond(where),
    kvtable(kvtable),
    types(types),
    order_by(order_by)
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
                s << " AS " + it3->first;
            }
            std::string k = it2->second.get<std::string>("key", "");
            if (k == "*")
                k = "";
            std::string v = it2->second.get<std::string>("value", "");
            if (v == "*")
                v = "";
            if (v == "" && k == "" && where_cond == "")
                s << " FROM " << tables;
            else
            {
                s << " FROM " << tables << " WHERE " << where_cond;
                if (k != "")
                {
                    if (where_cond != "")
                        s << " AND ";
                    s << kvtable << ".Key=" << "'" << k << "'";
                }
                if (v != "")
                {
                    if (where_cond != "" || k != "")
                        s << " AND ";
                    s << kvtable << ".Value=" << "'" << v << "'";
                }
            }
            el.put("query", s.str());
            chldrn.add_child("child", el);
        }
        chld.put_child("children", chldrn);
        retchldrn.add_child("child", chld);
    }
    ret.put_child("children", retchldrn);
    ret.put("ending", order_by);
    return ret;
}

} /* namespace sqllib */
