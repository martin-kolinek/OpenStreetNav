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

WayNodeFilter::WayNodeFilter(double dist_limit):
    dist_limit(dist_limit)
{
}

WayNodeFilter::~WayNodeFilter()
{
}

osm::Way WayNodeFilter::process_way(const osm::Way& w, const std::multimap<osm::Node, osm::Way, osm::LtByID>& ndmap)
{
    osm::Way ret(w.id);
    for (auto it = w.nodes.begin(); it != w.nodes.end(); ++it)
    {
        geo::Point last_point = (*w.nodes.begin()).second.position;
        if (it == w.nodes.begin() || has_important_ways(it->second, ndmap) || geo::get_point_distance(EARTH_RADIUS, last_point, it->second.position) > dist_limit || it == --w.nodes.end())
        {
            last_point = it->second.position;
            ret.add_node(it->second, it->first);
        }
    }
    return ret;
}

void WayNodeFilter::add_important(const std::string& key, const std::string& val)
{
    important.insert(std::make_pair(key, val));
}

osm::Way WayNodeFilter::process_way_pair(const std::pair<osm::Way, std::multimap<osm::Node, osm::Way, osm::LtByID> >& pair)
{
    return process_way(pair.first, pair.second);
}

bool WayNodeFilter::has_important_ways(const osm::Node& n, const std::multimap<osm::Node, osm::Way, osm::LtByID>& ndmap)
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
