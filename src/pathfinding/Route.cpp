#include "Route.h"
#include "../util/groupingiterator.h"
#include "../util/range.h"
#include "../util/materialize.h"

namespace pathfind
{
Route::Route()
{
}

Route::Route(std::vector<roads::RoadEdgeWithNodes> const& edges):
    edges(edges)
{
    tot_cost = 0;
    for (auto it = edges.begin(); it != edges.end(); ++it)
        tot_cost += it->cost;
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
        t.put("end_node", it->end_node.id);
        t.put("start_seq", it->start_seq_no);
        t.put("end_seq", it->end_seq_no);
        t.put("way", it->way.id);
        t.put("cost", it->cost);
        t.put("forward", it->forward);
        ret.add_child("edge", t);
    }
    return ret;
}

std::vector<std::shared_ptr<osm::HashElementContainer> > Route::get_highlighted() const
{

    auto ranges = edges | util::groupped(
                      [](roads::RoadEdgeWithNodes const & a, roads::RoadEdgeWithNodes const & b)
    {
        return a.way.id == b.way.id;
    },
    [](std::pair<int64_t, std::vector<std::pair<unsigned int, unsigned int> > >& res, roads::RoadEdgeWithNodes const & a)
    {
        res.first = a.way.id;
        res.second.push_back(std::make_pair(a.start_seq_no, a.end_seq_no));
    },
    std::pair<int64_t, std::vector<std::pair<unsigned int, unsigned int> > >()
                  );
    auto ranges2 = ranges | util::selected([](std::pair<int64_t, std::vector<std::pair<unsigned int, unsigned int> > > const & v)
    {
        return osm::WayRegion(osm::Way(v.first), v.second);
    }
                                          );
    std::vector<std::shared_ptr<osm::HashElementContainer> >  vect;
    for (auto it = ranges2.begin(); it != ranges2.end(); ++it)
    {
        vect.push_back(std::shared_ptr<osm::HashElementContainer>(new osm::WayRegion(*it)));
    }
    return vect;
}

std::vector<geo::Point> Route::get_points()
{
    std::vector<geo::Point> ret;
    for (auto it = edges.begin(); it != edges.end(); ++it)
    {
        ret.push_back(it->get_start_node().position);
        ret.push_back(it->get_end_node().position);
    }

    return ret;
}

double Route::total_cost()
{
    return tot_cost;
}

}
