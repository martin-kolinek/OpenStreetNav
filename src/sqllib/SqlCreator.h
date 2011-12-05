/*
 * SqlCreator.h
 *
 *  Created on: Dec 5, 2011
 *      Author: martin
 */

#ifndef SQLCREATOR_H_
#define SQLCREATOR_H_

#include <string>

namespace sqllib
{

class SqlCreator
{
public:
    virtual ~SqlCreator();
    virtual std::string create_sql() = 0;
};

} /* namespace sqllib */
#endif /* SQLCREATOR_H_ */
