#ifndef ROUTE_H_
#define ROUTE_H_

#include "../displayer/Descriptible.h"
#include "../roads/roads.h"

namespace pathfind
{

class Route : public display::Descriptible
{
public:
    Route();
    Route(std::vector<roads::RoadEdgeWithNodes> const& edges);
    ~Route();
    boost::property_tree::ptree get_description() const;
    std::vector<std::shared_ptr<osm::HashElementContainer> > get_highlighted() const;
    std::vector<geo::Point> get_points();
    double total_cost();
private:
    std::vector<roads::RoadEdgeWithNodes> edges;
    double tot_cost;
};

}

#endif
