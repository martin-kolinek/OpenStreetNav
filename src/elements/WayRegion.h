#ifndef WAYREGION_H_
#define WAYREGION_H_

#include "Way.h"
#include <vector>
#include <algorithm>

namespace osm
{

class Edge;

class WayRegion
{
public:
    WayRegion(osm::Way const& w);
    template<typename Rng>
    WayRegion(osm::Way const& w, Rng const& regions):
        w(w)
    {
        for (auto it = regions.begin(); it != regions.end(); ++it)
        {
            this->regions.push_back(*it);
        }
        std::sort(this->regions.begin(), this->regions.end());
    }

    bool intersects(Edge const& e) const;

    bool operator==(WayRegion const& other) const;
    bool operator!=(WayRegion const& other) const;
    bool operator<=(WayRegion const& other) const;
    bool operator>=(WayRegion const& other) const;
    bool operator>(WayRegion const& other) const;
    bool operator<(WayRegion const& other) const;
    osm::Way const& get_way() const;
private:
    osm::Way w;
    std::vector<std::pair<unsigned int, unsigned int> > regions;
};

}

#endif
