/*
 * ElementImporter.h
 *
 *  Created on: Jan 7, 2012
 *      Author: martin
 */

#ifndef ELEMENTIMPORTER_H_
#define ELEMENTIMPORTER_H_

#include "../elements/osmelements.h"

namespace osmdb
{

/**
 * \class ElementImporter
 * An interface responsible for inserting osm data into database.
 */
class ElementImporter
{
public:
    virtual void insert_node(osm::Node const& nd) = 0;
    virtual void insert_way(osm::Way const& w) = 0;
    virtual void insert_relation(osm::Relation const& rel) = 0;

    /**
     * This gets called automatically when importing relation so you shouldn't need to call it.
     * @param parent_id
     * @param role
     * @param child_id
     */
    virtual void insert_member_node(int64_t rel_id, std::string const& role, int64_t node_id) = 0;
    /**
     * This gets called automatically when importing relation so you shouldn't need to call it.
     * @param parent_id
     * @param role
     * @param child_id
     */
    virtual void insert_member_way(int64_t rel_id, std::string const& role, int64_t way_id) = 0;
    /**
     * This gets called automatically when importing relation so you shouldn't need to call it.
     * @param parent_id
     * @param role
     * @param child_id
     */
    virtual void insert_member_relation(int64_t parent_id, std::string const& role, int64_t child_id) = 0;
    virtual ~ElementImporter();
};

} /* namespace osmdb */
#endif /* ELEMENTIMPORTER_H_ */
