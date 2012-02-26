/*
 * WayLister.cpp
 *
 *  Created on: Jan 12, 2012
 *      Author: martin
 */

#include "WayLister.h"
#include "../sqllib/sqllib.h"
#include "../util/tuple_sub.h"
#include "../psql/psql.h"
#include "../util/groupingiterator.h"
#include "../util/SqlLibEntriesToPtree.h"
#include <tuple>

namespace osmdb
{

void test_changed(int64_t , int64_t , double , double , int64_t , const std::string& , const std::string& , int )
{
}

void cross_way_attr_comb(std::tuple<int64_t, int64_t, double, double, osm::Way>& res, const std::tuple<int64_t, int64_t, double, double, int64_t, std::string, std::string, int> & row)
{
    util::sub_tie<0, 1, 2, 3>(res) = util::sub_const_tie<0, 1, 2, 3>(row);
    std::get<4>(res).id = std::get<4>(row);
    if (std::get<5>(row) != "")
        std::get<4>(res).tags.insert(osm::Tag(std::get<5>(row), std::get<6>(row)));
}

void cross_way_comb(std::tuple<int64_t, osm::Node, std::vector<osm::Way> > & res, const std::tuple<int64_t, int64_t, double, double, osm::Way> & row)
{
    std::get<0>(res) = std::get<0>(row);
    osm::Node& nd = std::get<1>(res);
    nd.id = std::get<1>(row);
    nd.position.lon = std::get<2>(row);
    nd.position.lat = std::get<3> (row);
    if (std::get<4>(row).id != -1)
        std::get<2>(res).push_back(std::get<4>(row));
}

void node_comb(std::pair<osm::Way, std::multimap<osm::Node, osm::Way, osm::LtByID> > & res, const std::tuple<int64_t, osm::Node, std::vector<osm::Way> > & row)
{
    osm::Way& w = res.first;
    w.id = std::get<0>(row);
    w.nodes.push_back(std::get<1>(row));
    auto& mp = res.second;
    for (auto it = std::get<2>(row).begin(); it != std::get<2>(row).end(); ++it)
        mp.insert(std::make_pair(std::get<1>(row), *it));

}
WayLister::WayLister(OsmDatabase& db, const std::multimap<std::string, std::string> & attributes)
    : db(db)
{
    boost::property_tree::ptree ptree = util::get_entries(attributes);
    get_way_descr = psql::Cursor < psql::BindTypes<>, psql::RetTypes<int64_t, int64_t, double, double, int64_t, std::string, std::string, int> > (db.get_db(), "wayred_crs", sqllib::get_wayreduction_select(ptree, db.get_db()));
    get_way_descr.open();
    auto way_descr_rng = psql::make_cursor_range(get_way_descr);
    auto cross_way_attr_groupped = way_descr_rng |
                                   util::groupped(util::get_tuple_comparer<0, 1, 4>(), cross_way_attr_comb, std::tuple<int64_t, int64_t, double, double, osm::Way>());
    auto cross_ways_groupped = cross_way_attr_groupped |
                               util::groupped(util::get_tuple_comparer<0, 1>(), cross_way_comb, std::tuple<int64_t, osm::Node, std::vector<osm::Way> >());
    auto nodes_groupped = cross_ways_groupped |
                          util::groupped(util::get_tuple_comparer<0>(), node_comb, std::pair<osm::Way, std::multimap<osm::Node, osm::Way, osm::LtByID> >());
    rng = nodes_groupped;
}

WayLister::range_t WayLister::get_range()
{
    return rng;
}


WayLister::~WayLister()
{
}

} /* namespace osmdb */
