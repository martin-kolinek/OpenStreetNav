/*
 * SqlCreatorFactory.cpp
 *
 *  Created on: Dec 5, 2011
 *      Author: martin
 */

#include "SqlCreatorFactory.h"
#include "CompositeSqlCreator.h"
#include "SimpleSqlCreator.h"
#include <exception>

namespace sqllib
{

std::shared_ptr<SqlCreator> sqllib::SqlCreatorFactory::create(const boost::property_tree::ptree& tree)
{
    if (tree.get<std::string>("type") == "intersect" || tree.get<std::string>("type") == "union")
    {
        std::vector<std::shared_ptr<SqlCreator> > vect;
        for (auto it = tree.get_child("children").begin(); it != tree.get_child("children").end(); ++it)
        {
            vect.push_back(create(it->second));
        }
        std::string ending = tree.get<std::string>("ending", "");
        return std::shared_ptr<SqlCreator>(new CompositeSqlCreator(tree.get<std::string>("type"), vect, ending));
    }
    if (tree.get<std::string>("type") == "simple")
    {
        return std::shared_ptr<SqlCreator>(new SimpleSqlCreator(tree.get<std::string>("query")));
    }
    throw std::exception();
    return std::shared_ptr<SqlCreator>();
}

} /* namespace sqllib */
