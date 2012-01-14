/*
 * WayNodeFiltering.cpp
 *
 *  Created on: Jan 13, 2012
 *      Author: martin
 */

#include "WayNodeFilter.h"
#include <algorithm>

namespace wayred
{

WayNodeFilter::WayNodeFilter()
{
}

WayNodeFilter::~WayNodeFilter()
{
}

std::vector<osm::Way> WayNodeFilter::reduce_ways(const std::map<osm::Way, std::multimap<osm::Node, osm::Way, osm::LtByID> , osm::LtByID> & old_ways)
{
    std::vector<osm::Way> ret;
    for (auto it = old_ways.begin(); it != old_ways.end(); ++it)
    {
        ret.push_back(process_way(it->first, it->second));
    }
    return ret;
}

osm::Way WayNodeFilter::process_way(const osm::Way& w, const std::multimap<osm::Node, osm::Way, osm::LtByID> & ndmap)
{
    osm::Way ret(w.id);
    for (unsigned int i = 0; i < w.nodes.size(); ++i)
    {
        if (i == 0 || has_important_ways(w.nodes[i], ndmap) || i == w.nodes.size() - 1)
            ret.nodes.push_back(w.nodes[i]);
    }
    return ret;
}

void WayNodeFilter::add_important(const std::string& key, const std::string& val)
{
    important.insert(std::make_pair(key, val));
}

bool WayNodeFilter::has_important_ways(const osm::Node& n, const std::multimap<osm::Node, osm::Way, osm::LtByID> & ndmap)
{
    std::vector<std::pair<std::string, std::string> > vect(important.size());
    for (auto it = ndmap.lower_bound(n); it != ndmap.upper_bound(n); ++it)
    {
        auto it2 = std::set_intersection(it->second.tags.begin(), it->second.tags.end(), important.begin(), important.end(), vect.begin());
        if (it2 != vect.begin())
            return true;
    }
    return false;
}

} /* namespace wayred */
