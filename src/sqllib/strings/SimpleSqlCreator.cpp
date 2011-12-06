/*
 * SimpleSqlCreator.cpp
 *
 *  Created on: Dec 5, 2011
 *      Author: martin
 */

#include "SimpleSqlCreator.h"

namespace sqllib
{

}

sqllib::SimpleSqlCreator::SimpleSqlCreator(const std::string& sql):
    sql(sql)
{
}

std::string sqllib::SimpleSqlCreator::create_sql()
{
    return sql;
}

/* namespace sqllib */
