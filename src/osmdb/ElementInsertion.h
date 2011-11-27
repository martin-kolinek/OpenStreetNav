/*
 * ElementInsertion.h
 *
 *  Created on: Nov 6, 2011
 *      Author: martin
 */

#ifndef ELEMENTINSERTION_H_
#define ELEMENTINSERTION_H_

#include "OsmDatabase.h"
#include "../elements/osmelements.h"

namespace osmdb
{

/**
 * \class ElementInsertion
 * Responsible for inserting osm elements into database
 */
class ElementInsertion
{
public:
    /**
     *
     * @param db database connection
     */
    ElementInsertion(OsmDatabase& db);
    virtual ~ElementInsertion();
    /**
     * Inserts node into database
     * @param nd node to insert
     */
    void insert_node(osm::Node const& nd);
    /**
     * Inserts way into database
     * @param w way to insert
     */
    void insert_way(osm::Way const& w);
    /**
     * Inserts relation into database
     * @param rel relation to insert
     */
    void insert_relation(osm::Relation const& rel);
private:
    OsmDatabase& db;
    psql::Statement<psql::BindTypes<int64_t, double, double>, psql::RetTypes<> > node_ins;
    psql::Statement<psql::BindTypes<int64_t, int64_t, int64_t>, psql::RetTypes<> > edge_ins;
    psql::Statement<psql::BindTypes<int64_t, int64_t, int>, psql::RetTypes<> > waynode_ins;
    psql::Statement<psql::BindTypes<int64_t>, psql::RetTypes<> > way_ins;
    psql::Statement<psql::BindTypes<int64_t, std::string, std::string>, psql::RetTypes<> > node_attrs_ins;
    psql::Statement<psql::BindTypes<int64_t, std::string, std::string>, psql::RetTypes<> > way_attrs_ins;

};

}
#endif /* ELEMENTINSERTION_H_ */
