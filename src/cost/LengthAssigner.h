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
    std::vector<roads::RoadEdge> extract_edges(osm::Way const& reduced, osm::Way const& full);
    virtual ~LengthAssigner();
private:
    template<typename Rng>
    void insert_road_edges(std::vector<roads::RoadEdge>& v, Rng& r, int seq, int nextseq, osm::Way const& w)
    {
        double ln = 0;
        auto it = r.begin();
        assert(it != r.end());
        auto last_pos = it->second.position;
        for (++it; it != r.end(); ++it)
        {
            ln += geo::get_point_distance(EARTH_RADIUS, it->second.position, last_pos);
        }
        if (one_way(w))
        {
            v.push_back(roads::RoadEdge(w.id, seq, nextseq, get_dir(w), ln));
        }
        else
        {
            v.push_back(roads::RoadEdge(w.id, seq, nextseq, true, ln));
            v.push_back(roads::RoadEdge(w.id, seq, nextseq, false, ln));
        }
    }

    bool one_way(osm::Way const& w);
    bool get_dir(osm::Way const& w);
};

} /* namespace cost */
#endif /* LENGTHASSIGNER_H_ */
