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
    std::string insert_node_sql;
    std::string insert_way_sql;
    std::string insert_edge_sql;
    std::string insert_attr_sql;
    std::string insert_rel_contents_sql;
    std::string insert_relation_sql;
    sqlite::Statement attr_st;
    sqlite::Statement node_st;
    sqlite::Statement edge_st;
    sqlite::Statement way_st;
    sqlite::Statement member_st;
    sqlite::Statement rel_st;
    void insert_attributes(std::vector<osm::Tag> const& tags, int64_t id, osm::ObjectType tp);

};

}
#endif /* ELEMENTINSERTION_H_ */
