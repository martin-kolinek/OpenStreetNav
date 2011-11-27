#ifndef PSQL_H_
#define PSQL_H_

#include "Database.h"
#include "PgSqlException.h"
#include "Statement.h"
#include "RetTypes.h"
#include "BindTypes.h"

namespace psql
{

void execute_sql(Database& db, std::string const& sql);
template<typename... Types>
std::vector<std::tuple<Types...> > query_sql(Database& db, std::string const& sql)
{
    Statement<BindTypes<>, RetTypes<Types...> > st(sql, db);
    st.execute();
    std::vector<std::tuple<Types...> > ret;
    for (int i = 0; i < st.row_count(); ++i)
    {
        ret.push_back(st.get_row(i));
    }
    return ret;
}

}

#endif
