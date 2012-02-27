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
#include "../util/unpack_call.h"
#include "../util/make_ref.h"
#include <boost/range/adaptors.hpp>
#include <boost/range/any_range.hpp>

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
     * Process individual way.
     * @param w way to process
     * @param ndmap multimap which maps nodes to adjacent ways other than w
     * @return copy of w with filtered nodes
     */
    osm::Way process_way(osm::Way const& w, std::multimap<osm::Node, osm::Way, osm::LtByID> const& ndmap);
    osm::Way process_way_pair(std::pair<osm::Way, std::multimap<osm::Node, osm::Way, osm::LtByID> > const& pair);
    /**
     *
     * @param input a range of pairs
     * @return range of reduced ways
     */
    template<typename Rng>
    boost::any_range<osm::Way, boost::single_pass_traversal_tag, osm::Way, size_t> process_range(Rng const& rng)
    {
        return rng |
               boost::adaptors::transformed(boost::bind(&WayNodeFilter::process_way_pair, this, _1));
    }

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
