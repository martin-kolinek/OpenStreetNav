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

class ElementImporter
{
public:
    virtual void insert_node(osm::Node const& nd) = 0;
    virtual void insert_way(osm::Way const& w) = 0;
    virtual void insert_relation(osm::Relation const& rel) = 0;

    virtual void insert_member_node(int64_t rel_id, std::string const& role, int64_t node_id) = 0;
    virtual void insert_member_way(int64_t rel_id, std::string const& role, int64_t way_id) = 0;
    virtual void insert_member_relation(int64_t parent_id, std::string const& role, int64_t child_id) = 0;
    virtual ~ElementImporter();
};

} /* namespace osmdb */
#endif /* ELEMENTIMPORTER_H_ */
