#ifndef SQLLIBENTRIESTOPTREE_H_
#define SQLLIBENTRIESTOPTREE_H_

#include <boost/property_tree/ptree.hpp>
#include <string>
#include <map>

namespace util
{

boost::property_tree::ptree get_entries(const std::multimap<std::string, std::string> & attributes);

}

#endif
