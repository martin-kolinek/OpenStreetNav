/*
 * SqlCreatorFactory.h
 *
 *  Created on: Dec 5, 2011
 *      Author: martin
 */

#ifndef SQLCREATORFACTORY_H_
#define SQLCREATORFACTORY_H_

#include <boost/property_tree/ptree.hpp>

namespace sqllib
{

class SqlCreatorFactory
{
public:
    static std::shared_ptr<SqlCreator> create(boost::property_tree::ptree const& tree);
};

} /* namespace sqllib */
#endif /* SQLCREATORFACTORY_H_ */
