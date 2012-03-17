/*
 * ToShowSelectCollection.h
 *
 *  Created on: Dec 7, 2011
 *      Author: martin
 */

#ifndef TOSHOWSELECTCOLLECTION_H_
#define TOSHOWSELECTCOLLECTION_H_

#include <string>
#include "../psql/psql.h"

namespace osmdb
{

/**
 * \class ToShowSelectCollection
 * Maintains a collection of select statements used for retrieving data to display from database.
 * Uses xml files
 */
class ToShowSelectCollection
{
public:
    /**
     * Constructor
     * @param path_base path to directory containing <min>.xml to <max>.xml
     * @param db database connection to use for statements
     * @param min minimum zoom to look for
     * @param max maximum zoom to look for
     */
    ToShowSelectCollection(std::string const& path_base, psql::Database& db, int min, int max);
    /**
     *
     * @param zoom
     * @return Statement to use to get drawing information for zoom
     */
    psql::Statement<psql::BindTypes<double, double, double, double>, psql::RetTypes<int, int64_t, double, double, int, int64_t, double, double, int64_t, double, double, double, double, double, int, int> >& get_edges_for_zoom(int zoom);
    /**
     *
     * @param zoom
     * @return Statement to use to get selected element information for zoom
     */
    psql::Statement<psql::BindTypes<double, double, double, double>, psql::RetTypes<int64_t, double, double, double, double, double, int, int> >& get_select_edges(int zoom);
private:
    std::vector<psql::Statement<psql::BindTypes<double, double, double, double>, psql::RetTypes<int, int64_t, double, double, int, int64_t, double, double, int64_t, double, double, double, double, double, int, int> > > statements;
    std::vector<psql::Statement<psql::BindTypes<double, double, double, double>, psql::RetTypes<int64_t, double, double, double, double, double, int, int> > > select_statements;
    int offset;
};

} /* namespace osmdb */
#endif /* TOSHOWSELECTCOLLECTION_H_ */
