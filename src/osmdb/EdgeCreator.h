/*
 * EdgeCreator.h
 *
 *  Created on: Mar 18, 2012
 *      Author: martin
 */

#ifndef EDGECREATOR_H_
#define EDGECREATOR_H_

#include "OsmDatabase.h"

namespace osmdb
{

class EdgeCreator
{
public:
    EdgeCreator(OsmDatabase& db);
    void create_tables();
    void create_keys_and_indexes();
    void insert_data();
    void drop_keys_and_indexes();
    virtual ~EdgeCreator();
private:
    OsmDatabase& db;
};

} /* namespace osmdb */
#endif /* EDGECREATOR_H_ */
