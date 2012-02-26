/*
 * WayLister.h
 *
 *  Created on: Jan 12, 2012
 *      Author: martin
 */

#ifndef WAYLISTER_H_
#define WAYLISTER_H_

#include "OsmDatabase.h"
#include <string>
#include "../elements/osmelements.h"
#include "../psql/psql.h"
#include <boost/range/any_range.hpp>

namespace osmdb
{

/**
 * \class WayLister
 * Responsible for retrieving ways for the purpose of filtering member nodes.
 */
class WayLister
{
public:
    typedef std::pair<osm::Way, std::multimap<osm::Node, osm::Way, osm::LtByID> > value_t;
    typedef std::pair<osm::Way, std::multimap<osm::Node, osm::Way, osm::LtByID> > const& reference_t;
    typedef boost::any_range<value_t, boost::single_pass_traversal_tag, reference_t, size_t> range_t;
    /**
     * Constructor
     * @param db underlying OsmDatabase
     * @param attributes ways containing these attributes will be fetchet
     * @param fetch_size amount of rows to fetch in one chunk
     */
    WayLister(OsmDatabase& db, std::multimap<std::string, std::string> const& attributes);

    range_t get_range();

    virtual ~WayLister();
private:
    OsmDatabase& db;
    psql::Cursor<psql::BindTypes<>, psql::RetTypes<int64_t, int64_t, double, double, int64_t, std::string, std::string, int> > get_way_descr;
    range_t rng;
};

} /* namespace osmdb */
#endif /* WAYLISTER_H_ */
