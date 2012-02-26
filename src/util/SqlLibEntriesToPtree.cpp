#include "SqlLibEntriesToPtree.h"

namespace util
{

boost::property_tree::ptree get_entries(const std::multimap<std::string, std::string> & attributes)
{
    boost::property_tree::ptree ret;
    boost::property_tree::ptree entries;
    for (auto it = attributes.begin(); it != attributes.end(); ++it)
    {
        boost::property_tree::ptree entry;
        boost::property_tree::ptree kv;
        kv.put("key", it->first);
        kv.put("value", it->second);
        entry.put_child("elements.el", kv);
        entries.add_child("entry", entry);
    }
    ret.add_child("entries", entries);
    return ret;
}

}
