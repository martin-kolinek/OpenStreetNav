/*
 * RoadNetworkCreator.cpp
 *
 *  Created on: Mar 3, 2012
 *      Author: martin
 */

#include "RoadNetworkCreator.h"
#include "../sqllib/sqllib.h"
#include "../util/groupingiterator.h"
#include "AllWayLister.h"
#include "../cost/LengthAssigner.h"

namespace osmdb
{

RoadNetworkCreator::RoadNetworkCreator(OsmDatabase& full, OsmDatabase& reduced, OsmDatabase& destination, std::multimap<std::string, std::string> const& attributes):
    full(full),
    reduced(reduced),
    destination(destination),
    attributes(attributes)
{
}

void RoadNetworkCreator::create_road_network_table()
{
    sqllib::get_create_road_edges_table(destination.get_db()).execute();
    sqllib::get_create_road_edges_pkey(destination.get_db()).execute();
    sqllib::get_create_road_edges_fkey(destination.get_db()).execute();
    sqllib::get_create_road_edges_view(destination.get_db()).execute();
}

void group_way_nodes(osm::Way& w, std::tuple<int64_t, int64_t, int> const& row)
{
    w.id = std::get<0>(row);
    w.nodes.push_back(osm::Node(std::get<1>(row)));
}

std::pair<osm::Way, osm::Way> comb(osm::Way const& w1, osm::Way const& w2)
{
    return std::make_pair(w1, w2);
}

void RoadNetworkCreator::copy_road_network_data()
{
    sqllib::get_drop_road_edges_fkey(destination.get_db()).execute();
    auto st = sqllib::get_copy_road_network(destination.get_db());
    st.execute();
    auto red_st = sqllib::get_select_ways_with_nodes(reduced.get_db());
    auto reduced_rows = psql::exec_statement(red_st);
    auto reduced_ways = reduced_rows |
                        util::groupped(util::get_tuple_comparer<0>(), group_way_nodes, osm::Way(0));
    full.get_db().begin_transaction();
    AllWayLister list(full, attributes);
    auto combined = util::sorted_combine(reduced_ways, list.get_range(), comb, osm::LtByID());
    cost::LengthAssigner la;

    for (auto it = combined.begin(); it != combined.end(); ++it)
    {
        auto v = la.extract_edges(it->first, it->second);
        for (auto it2 = v.begin(); it2 != v.end(); ++it2)
        {
            st.copy_data(it2->way_id, it2->seq_no, it2->forward, it2->cost);
        }
    }
    full.get_db().rollback_transaction();
    st.end_copy();
    sqllib::get_create_road_edges_fkey(destination.get_db()).execute();
}

RoadNetworkCreator::~RoadNetworkCreator()
{
}

} /* namespace util */
