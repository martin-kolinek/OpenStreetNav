#ifndef PSQL_H_
#define PSQL_H_

#include "Database.h"
#include "PgSqlException.h"
#include "Statement.h"
#include "RetTypes.h"
#include "BindTypes.h"
#include "Cursor.h"
#include <type_traits>
#include <tuple>

namespace psql
{

/**
 * Utility function which constructs a Statement with no arguments and executes it.
 * @param db database connection to use
 * @param sql sql query to execute
 */
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

/**
 * Wrapper which calls execute on a Statement and aggregates rows into a std::vector
 * @param st statement to execute
 * @param bvs arguments to supply to execute
 * @return std::vector of rows
 */
template<typename... BTypes, typename... RTypes>
std::vector<std::tuple<RTypes...> > exec_statement(psql::Statement<psql::BindTypes<BTypes...>, psql::RetTypes<RTypes...> >& st, BTypes... bvs)
{
    std::vector<std::tuple<RTypes...> > ret;
    st.execute(bvs...);
    ret.reserve(st.row_count());
    for (int i = 0; i < st.row_count(); ++i)
    {
        ret.push_back(st.get_row(i));
    }
    return ret;
}

/**
 * Wrapper which extracts a column from Statement results.
 * @param st
 * @param bvs
 * @return vector of values in column Col after executing statement st with parameters bvs
 */
template < unsigned int Col = 0, typename... BTypes, typename... RTypes >
auto exec_statement_col(psql::Statement<psql::BindTypes<BTypes...>, psql::RetTypes<RTypes...> >& st, BTypes... bvs)
-> std::vector<typename std::remove_const<typename std::remove_reference<decltype(std::get<Col>(st.get_row(0)))>::type>::type>
{
    std::vector<typename std::remove_const<typename std::remove_reference<decltype(std::get<Col>(st.get_row(0)))>::type>::type> ret;
    st.execute(bvs...);
    ret.reserve(st.row_count());
    for (int i = 0; i < st.row_count(); ++i)
    {
        ret.push_back(std::get<Col>(st.get_row(i)));
    }
    return ret;
}

}

#endif
