/*
 * RoadLister.cpp
 *
 *  Created on: Mar 21, 2012
 *      Author: martin
 */

#include "RoadLister.h"
#include "../sqllib/sqllib.h"
#include <tuple>

namespace osmdb
{

RoadLister::RoadLister(OsmDatabase& db):
    db(db)
{
}

RoadLister::~RoadLister()
{
}

std::vector<roads::RoadEdgeWithNodes> RoadLister::get_edges()
{
    auto st = sqllib::get_select_road_edges(db.get_db());
    auto v = psql::exec_statement(st);
    std::vector<roads::RoadEdgeWithNodes> ret;
    ret.reserve(v.size());
    for (auto it = v.begin(); it != v.end(); ++it)
    {
        int64_t n1id, n2id, wid;
        int startseqno, endseqno;
        bool forward;
        double x1, y1, x2, y2, cost;
        std::tie(n1id, x1, y1, n2id, x2, y2, wid, startseqno, endseqno, forward, cost) = *it;
        ret.push_back(roads::RoadEdgeWithNodes(wid, startseqno, endseqno, forward, cost, osm::Node(n1id, y1, x1), osm::Node(n2id, y2, x2)));
    }
    return ret;
}

void RoadLister::fill_road_network(roads::RoadNetwork& net)
{
    auto rng = get_edges();
    for (auto it = rng.begin(); it != rng.end(); ++it)
    {
        net.add_edge(*it);
    }
}

} /* namespace osmdb */
