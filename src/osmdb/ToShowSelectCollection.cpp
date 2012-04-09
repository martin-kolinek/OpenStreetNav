/*
 * ToShowSelectCollection.cpp
 *
 *  Created on: Dec 7, 2011
 *      Author: martin
 */

#include "ToShowSelectCollection.h"
#include <boost/property_tree/ptree.hpp>
#include <boost/property_tree/xml_parser.hpp>
#include "../sqllib/sqllib.h"

namespace osmdb
{

ToShowSelectCollection::ToShowSelectCollection(std::vector<std::string> const& schemas, int offset, psql::Database& db):
    offset(offset)
{
    std::string sch = db.get_schema();
    int i = 0;
    for (auto it = schemas.begin(); it != schemas.end(); ++it)
    {
        db.set_schema(*it);
        statements.push_back(sqllib::get_select_edges_in_box(db, true, util::concatenate("", "select_edge_stmt", i++)));
        select_statements.push_back(sqllib::get_select_edges_in_exact_box(db, true, util::concatenate("", "select_edge_stmt2", i++)));
    }
    db.set_schema(sch);
}

psql::Statement<psql::BindTypes<double, double, double, double>, psql::RetTypes<int, int64_t, double, double, int, int64_t, double, double, int64_t, double, double, double, double, double, int, int> >& ToShowSelectCollection::get_edges_for_zoom(int zoom)
{
    return statements[zoom - offset];
}

psql::Statement<psql::BindTypes<double, double, double, double>, psql::RetTypes<int64_t, int, int64_t, int, int64_t> >& ToShowSelectCollection::get_select_statement(int zoom)
{
    return select_statements[zoom - offset];
}

}


/* namespace osmdb */
