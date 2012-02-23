#include "AllWayLister.h"
#include "../util/SqlLibEntriesToPtree.h"
#include "../sqllib/sqllib.h"

namespace osmdb
{

AllWayLister::AllWayLister(OsmDatabase& db, std::multimap<std::string, std::string> const& attributes)
{
    boost::property_tree::ptree tr = util::get_entries(attributes);
    way_attr_crs = psql::make_cursor(db.get_db(), "way_attr_crs", sqllib::get_way_attributes_select(tr, db.get_db()));
    way_node_attr_crs = psql::make_cursor(db.get_db(), "way_node_attr_crs", sqllib::get_way_node_attributes_select(tr, db.get_db()));
}

void combine(osm::Way& w, std::tuple<int64_t, std::string, std::string> const& row)
{
    int64_t id;
    std::string k, v;
    std::tie(id,k,v)=row;
    w.id=id;
    w.tags.insert(osm::Tag(k,v));
}

void combine_node(std::tuple<int64_t, osm::Node>& acc, std::tuple<int64_t, int64_t, std::string, std::string> const& row)
{
    std::get<0>(acc)=std::get<0>(row);
    std::get<1>(acc).id = std::get<1>(row);
    std::get<1>(acc).tags.insert(osm::Tag(std::get<2>(row), std::get<3>(row)));
}

void combine_way(osm::Way& w, std::tuple<int64_t, osm::Node> const& row)
{
    w.id=std::get<0>(row);
    w.nodes.push_back(std::get<1>(row));
}


decltype(util::sorted_combine(std::vector<osm::Way>(), std::vector<osm::Way>())) AllWayLister::get_range() 
{
    auto way_attr_rng = make_cursor_range(way_attr_crs);
    auto ways_with_attrs = way_attr_rng | groupped(combine, osm::Way());
    auto way_node_attr_rng = make_cursor_range(way_node_attr_crs);
    auto way_with_nodes = way_node_attr_rng | 
        util::groupped(combine_node, std::tuple<int64_t, osm::Node>()) |
        util::groupped(combine_way, osm::Way());
    return util::sorted_combine(way_with_attrs, way_with_nodes, comb_way, osm::LtByID);
}

}
