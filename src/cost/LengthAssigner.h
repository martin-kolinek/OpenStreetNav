/*
 * LengthAssigner.h
 *
 *  Created on: Mar 4, 2012
 *      Author: martin
 */

#ifndef LENGTHASSIGNER_H_
#define LENGTHASSIGNER_H_

#include "CostAssigner.h"

namespace cost
{

class LengthAssigner : public CostAssigner
{
public:
    LengthAssigner();
    std::vector<roads::RoadEdgeWithNodes> extract_edges(osm::Way const& reduced, osm::Way const& full);
    virtual ~LengthAssigner();
private:
    template<typename Rng, typename It>
    void insert_road_edges(std::vector<roads::RoadEdgeWithNodes>& v, Rng& r, It start, It end, osm::Way const& w)
    {
        double ln = 0;
        auto it = r.begin();
        assert(it != r.end());
        auto last_pos = it->second.position;
        for (++it; it != r.end(); ++it)
        {
            ln += geo::get_point_distance(EARTH_RADIUS, it->second.position, last_pos);
            last_pos = it->second.position;
        }
        if (one_way(w))
        {
            v.push_back(roads::RoadEdgeWithNodes(w.id, start->first, end->first, get_dir(w), ln, start->second, end->second));
        }
        else
        {
            v.push_back(roads::RoadEdgeWithNodes(w.id, start->first, end->first, true, ln, start->second, end->second));
            v.push_back(roads::RoadEdgeWithNodes(w.id, start->first, end->first, false, ln, start->second, end->second));
        }
    }

    bool one_way(osm::Way const& w);
    bool get_dir(osm::Way const& w);
};

} /* namespace cost */
#endif /* LENGTHASSIGNER_H_ */
