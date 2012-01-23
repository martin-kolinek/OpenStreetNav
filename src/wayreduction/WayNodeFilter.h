/*
 * WayNodeFiltering.h
 *
 *  Created on: Jan 13, 2012
 *      Author: martin
 */

#ifndef WAYNODEFILTER_H_
#define WAYNODEFILTER_H_

#include <vector>
#include "../elements/osmelements.h"
#include <map>

namespace wayred
{

/**
 * \class WayNodeFilter
 * Takes care of filtering out nodes that are unimportant. That is there are no important ways connected to them.
 */
class WayNodeFilter
{
public:
    WayNodeFilter();
    virtual ~WayNodeFilter();
    /**
     * Process ways in a batch.
     * Keys in old_ways are the ways from which to remove nodes, values are multimaps that map nodes to other adjacent ways
     * @param old_ways
     * @return std::vector of ways with nodes removed
     */
    std::vector<osm::Way> reduce_ways(std::map<osm::Way, std::multimap<osm::Node, osm::Way, osm::LtByID>, osm::LtByID> const& old_ways);
    /**
     * Process individual way.
     * @param w way to process
     * @param ndmap multimap which maps nodes to adjacent ways other than w
     * @return copy of w with filtered nodes
     */
    osm::Way process_way(osm::Way const& w, std::multimap<osm::Node, osm::Way, osm::LtByID> const& ndmap);
    /**
     * Add an attribute to be considered important way.
     * @param key
     * @param val
     */
    void add_important(std::string const& key, std::string const& val);
private:
    std::set<std::pair<std::string, std::string> > important;
    bool has_important_ways(osm::Node const& n, std::multimap<osm::Node, osm::Way, osm::LtByID> const& ndmap);
};

} /* namespace wayred */
#endif /* WAYNODEFILTER_H_ */
