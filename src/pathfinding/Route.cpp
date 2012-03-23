#include "Route.h"

namespace pathfind
{
Route::Route()
{
}

Route::Route(std::vector<roads::RoadEdgeWithNodes> const& edges):
    edges(edges)
{

}

Route::~Route()
{
}

boost::property_tree::ptree Route::get_description() const
{
    boost::property_tree::ptree ret;
    if (edges.size() == 0)
    {
        ret.put("Problem", "No route found");
    }

    for (auto it = edges.begin(); it != edges.end(); ++it)
    {
        boost::property_tree::ptree t;
        t.put("start_node", it->start_node.id);
        t.put("end_node", it->start_node.id);
        t.put("start_seq", it->start_node.id);
        t.put("end_seq", it->start_node.id);
        t.put("way", it->start_node.id);
        t.put("cost", it->cost);
        t.put("forward", it->forward);
        ret.add_child("edge", t);
    }
    return ret;
}

std::vector<std::unique_ptr<osm::WayRegion> > Route::get_regions() const
{
    std::vector<std::unique_ptr<osm::WayRegion> > regions;
    regions.reserve(edges.size());
    for (auto it = edges.begin(); it != edges.end(); ++it)
    {
        regions.push_back(std::unique_ptr<osm::WayRegion>(new osm::WayRegion(it->way_id, std::vector<std::pair<int, int> > {std::make_pair(it->start_seq_no, it->end_seq_no)})));
    }
    return regions;
}
}
