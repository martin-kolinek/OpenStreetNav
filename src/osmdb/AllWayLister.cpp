#include "AllWayLister.h"
#include "../util/SqlLibEntriesToPtree.h"
#include "../sqllib/sqllib.h"
#include "../util/groupingiterator.h"
#include "../util/tuple_sub.h"
#include "../util/materialize.h"

namespace osmdb
{

void combine(osm::Way& w, std::tuple<int64_t, std::string, std::string> const& row)
{
    int64_t id;
    std::string k, v;
    std::tie(id, k, v) = row;
    w.id = id;
    if (k != "")
        w.tags.insert(osm::Tag(k, v));
}

void combine_node(std::tuple<int64_t, osm::Node>& acc, std::tuple<int64_t, int64_t, double, double, std::string, std::string, int> const& row)
{
    std::get<0>(acc) = std::get<0>(row);
    std::get<1>(acc).id = std::get<1>(row);
    std::get<1>(acc).position.lon = std::get<2>(row);
    std::get<1>(acc).position.lat = std::get<3>(row);
    if (std::get<4>(row) != "")
        std::get<1>(acc).tags.insert(osm::Tag(std::get<4>(row), std::get<5>(row)));
}

void combine_way(osm::Way& w, std::tuple<int64_t, osm::Node> const& row)
{
    w.id = std::get<0>(row);
    w.nodes.push_back(std::get<1>(row));
}

osm::Way final_comb(osm::Way const& w1, osm::Way const& w2)
{
    osm::Way ret(w1.id);
    ret.nodes = w2.nodes;
    ret.tags = w1.tags;
    return ret;
}

AllWayLister::AllWayLister(OsmDatabase& db, std::multimap<std::string, std::string> const& attributes)
{
    boost::property_tree::ptree tr = util::get_entries(attributes);
    way_attr_crs = psql::make_cursor(db.get_db(), "way_attr_crs", sqllib::get_way_attributes_select(tr, db.get_db()));
    way_node_attr_crs = psql::make_cursor(db.get_db(), "way_node_attr_crs", sqllib::get_way_node_attributes_select(tr, db.get_db()));
    way_attr_crs.open();
    way_node_attr_crs.open();
    auto way_attr_rng = make_cursor_range(way_attr_crs);
    auto ways_with_attrs = way_attr_rng | util::groupped(util::get_tuple_comparer<0>(), combine, osm::Way());

    auto way_node_attr_rng = make_cursor_range(way_node_attr_crs);

    auto way_with_nodes = way_node_attr_rng |
                          util::groupped(util::get_tuple_comparer<0, 1>(), combine_node, std::tuple<int64_t, osm::Node>());

    auto way_with_nodes2 = way_with_nodes |
                           util::groupped(util::get_tuple_comparer<0>(), combine_way, osm::Way());

    rng = util::sorted_combine(ways_with_attrs, way_with_nodes2, final_comb, osm::LtByID());
}

boost::any_range<osm::Way, boost::single_pass_traversal_tag, osm::Way const&, int> const& AllWayLister::get_range()
{
    return rng;
}

}
