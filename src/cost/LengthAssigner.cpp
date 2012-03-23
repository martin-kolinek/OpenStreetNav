/*
 * LengthAssigner.cpp
 *
 *  Created on: Mar 4, 2012
 *      Author: martin
 */

#include "LengthAssigner.h"
#include <boost/range.hpp>
#include <boost/range/any_range.hpp>
#include "../util/tuple_sub.h"
#include "../util/range.h"

namespace cost
{

LengthAssigner::LengthAssigner()
{

}

std::vector<roads::RoadEdgeWithNodes> LengthAssigner::extract_edges(osm::Way const& reduced, osm::Way const& full)
{
    std::vector<roads::RoadEdgeWithNodes> ret;
    boost::any_range<std::pair<const int, osm::Node>, boost::forward_traversal_tag, std::pair<const int, osm::Node>, size_t> rng = full.nodes;
    auto it = reduced.nodes.begin();
    auto it2 = it;
    for (++it; it != reduced.nodes.end(); ++it, ++it2)
    {
        auto pair = util::split(rng, boost::bind(util::get_tuple_comparer<0>(), _1, *it));
        insert_road_edges(ret, pair.first, it2, it, full);
        rng = pair.second;
    }

    return ret;
}

bool LengthAssigner::one_way(osm::Way const& w)
{
    auto one_way_tag = osm::get_tag(w.tags, "oneway");
    if (!one_way_tag)
    {
        auto highway_tag = osm::get_tag(w.tags, "highway");
        if (!highway_tag)
            return false;
        return boost::range::count(util::gen_vect("primary_link", "trunk_link", "motorway_link", "roundabout", "motorway"), highway_tag->second);
    }
    return boost::range::count(util::gen_vect("yes", "1", "true", "-1", "reverse"), one_way_tag->second);
}

bool LengthAssigner::get_dir(osm::Way const& w)
{
    auto one_way_tag = osm::get_tag(w.tags, "oneway");
    if (!one_way_tag)
        return false;
    return !(boost::range::count(util::gen_vect("-1", "reverse"), one_way_tag->second));
}

LengthAssigner::~LengthAssigner()
{
}

} /* namespace cost */
