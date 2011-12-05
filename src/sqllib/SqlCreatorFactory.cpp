/*
 * SqlCreatorFactory.cpp
 *
 *  Created on: Dec 5, 2011
 *      Author: martin
 */

#include "SqlCreatorFactory.h"

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
        return std::shared_ptr<SqlCreator>(new CompositeSqlCreator(tree.get<std::string>("type"), vect));
    }
    if (tree.get<std::string>("type") == "simple")
    {
        return std::shared_ptr<SqlCreator>(new SimpleSqlCreator(tree.get<std::string>("query")));
    }
    std::assert(false);
    return std::shared_ptr<SqlCreator>();
}

} /* namespace sqllib */
