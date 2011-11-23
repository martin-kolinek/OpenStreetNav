/*
 * sqllib.h
 *
 *  Created on: Nov 23, 2011
 *      Author: martin
 */

#ifndef SQLLIB_H_
#define SQLLIB_H_

#include "../psql/psql.h"
#include <string>

namespace sqllib
{

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_create_test_table(psql::Database& db, bool named = false, std::string const& name = "");

psql::Statement<psql::BindTypes<int, std::string, int64_t>, psql::RetTypes<>> get_insert_test_table(psql::Database& db, bool named = false, std::string const& name = "");

psql::Statement<psql::BindTypes<int>, psql::RetTypes<int>> get_test_select(psql::Database& db, bool named = false, std::string const& name = "");


}

#endif
