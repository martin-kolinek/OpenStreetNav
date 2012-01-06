/*
 * KeyValFilterTranslator.h
 *
 *  Created on: Dec 5, 2011
 *      Author: martin
 */

#ifndef KEYVALFILTERTRANSLATOR_H_
#define KEYVALFILTERTRANSLATOR_H_

#include <boost/property_tree/ptree.hpp>

namespace sqllib
{

class KeyValFilterTranslator
{
public:
    KeyValFilterTranslator(std::string const& cols, std::string const& tables, std::string const& where, std::string const& kvtable, std::vector<std::string> const& types, std::string const& order_by);
    boost::property_tree::ptree translate(boost::property_tree::ptree const& input);
private:
    std::string cols;
    std::string tables;
    std::string where_cond;
    std::string kvtable;
    std::string order_by;
    std::vector<std::string> types;
};

} /* namespace sqllib */
#endif /* KEYVALFILTERTRANSLATOR_H_ */
