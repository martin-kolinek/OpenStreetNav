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
    EdgeCreator(OsmDatabase& db, boost::property_tree::ptree entries);
    void create_tables();
    void create_keys_and_indexes();
    void insert_data();
    void drop_keys_and_indexes();
    virtual ~EdgeCreator();
private:
    OsmDatabase& db;
    boost::property_tree::ptree entries;
};

} /* namespace osmdb */
#endif /* EDGECREATOR_H_ */
