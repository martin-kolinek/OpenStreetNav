/*
 * psql.cpp
 *
 *  Created on: Nov 27, 2011
 *      Author: martin
 */

#include "BindTypes.h"
#include "RetTypes.h"
#include "psql.h"

namespace psql
{

void execute_sql(Database& db, std::string const& sql)
{
    Statement<BindTypes<>, RetTypes<> > st(sql, db);
    st.execute();
}

}

