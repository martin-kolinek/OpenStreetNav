/*
 * sqllib.cc
 *
 *  Created on: Nov 23, 2011
 *      Author: martin
 */

#include "sqllib.h"

namespace sqllib
{

psql::Statement<psql::BindTypes<>, psql::RetTypes<>> get_create_test_table(psql::Database& db, bool named, std::string const& name)
{
    std::string str("\
CREATE TABLE TestTable (A int primary key, B text, C bigint)\n\
");
    if (named)
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(str, name, db);
    else
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<>>(str, db);
}

psql::Statement<psql::BindTypes<int, std::string, int64_t>, psql::RetTypes<>> get_insert_test_table(psql::Database& db, bool named, std::string const& name)
{
    std::string str("\
INSERT INTO TestTable (A, B, C) VALUES ($1, $2, $3)\n\
");
    if (named)
        return psql::Statement<psql::BindTypes<int, std::string, int64_t>, psql::RetTypes<>>(str, name, db);
    else
        return psql::Statement<psql::BindTypes<int, std::string, int64_t>, psql::RetTypes<>>(str, db);
}

psql::Statement<psql::BindTypes<int>, psql::RetTypes<int, std::string, int64_t>> get_test_select(psql::Database& db, bool named, std::string const& name)
{
    std::string str("\
\n\
SELECT * FROM TestTable\n\
WHERE\n\
--comment\n\
A = $1;\n\
\n\
");
    if (named)
        return psql::Statement<psql::BindTypes<int>, psql::RetTypes<int, std::string, int64_t>>(str, name, db);
    else
        return psql::Statement<psql::BindTypes<int>, psql::RetTypes<int, std::string, int64_t>>(str, db);
}


}
