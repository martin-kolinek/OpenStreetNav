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
    std::vector<std::unique_ptr<osm::WayRegion> > get_regions() const;
private:
    std::vector<roads::RoadEdgeWithNodes> edges;
};

}

#endif
