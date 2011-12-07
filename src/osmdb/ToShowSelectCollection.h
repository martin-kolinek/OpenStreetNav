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

class ToShowSelectCollection
{
public:
    ToShowSelectCollection(std::string const& path_base, psql::Database& db, int min, int max);
    psql::Statement<psql::BindTypes<double, double, double, double>, psql::RetTypes<double, double, double, double, int64_t, double, double, double, double, int, int> >& get_edges_for_zoom(int zoom);
private:
    std::vector<psql::Statement<psql::BindTypes<double, double, double, double>, psql::RetTypes<double, double, double, double, int64_t, double, double, double, double, int, int> > > statements;
    int offset;
};

} /* namespace osmdb */
#endif /* TOSHOWSELECTCOLLECTION_H_ */
