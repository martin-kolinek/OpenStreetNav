/*
 * ElementCopy.h
 *
 *  Created on: Jan 7, 2012
 *      Author: martin
 */

#ifndef ELEMENTCOPY_H_
#define ELEMENTCOPY_H_

#include "ElementImporter.h"
#include "OsmDatabase.h"

namespace osmdb
{

/**
 * \class ElementCopy
 * Handles copying into the Import table of the database.
 */
class ElementCopy : public ElementImporter
{
public:
    /**
     * Constructor
     * @param db underlying OsmDatabase to use.
     */
    ElementCopy(OsmDatabase& db);
    void start_copy();
    void end_copy();
    void insert_node(osm::Node const& nd);
    void insert_way(osm::Way const& w);
    void insert_relation(osm::Relation const& rel);

    void insert_member_node(int64_t rel_id, std::string const& role, int64_t node_id);
    void insert_member_way(int64_t rel_id, std::string const& role, int64_t way_id);
    void insert_member_relation(int64_t parent_id, std::string const& role, int64_t child_id);

    virtual ~ElementCopy();
private:
    OsmDatabase& db;
    psql::Statement<psql::BindTypes<>, psql::RetTypes<>, psql::CopyTypes<int, int64_t, int64_t, int, int, double, double, std::string, std::string> > copy;
};

} /* namespace osmdb */
#endif /* ELEMENTCOPY_H_ */
