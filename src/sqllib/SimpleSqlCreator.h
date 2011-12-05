/*
 * SimpleSqlCreator.h
 *
 *  Created on: Dec 5, 2011
 *      Author: martin
 */

#ifndef SIMPLESQLCREATOR_H_
#define SIMPLESQLCREATOR_H_

#include "SqlCreator.h"

namespace sqllib
{

class SimpleSqlCreator : public SqlCreator
{
public:
    SimpleSqlCreator(std::string const& sql);
    std::string create_sql();
private:
    std::string sql;
};

} /* namespace sqllib */
#endif /* SIMPLESQLCREATOR_H_ */
