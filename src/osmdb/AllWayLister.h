#ifndef ALLWAYLISTER_H_
#define ALLWAYLISTER_H_

#include "../elements/osmelements.h"
#include "../util/sortedcombiterator.h"
#include "OsmDatabase.h"
#include <map>
#include <string>
#include <vector>

namespace osmdb
{
    namespace AllWayListerDetail
    {
        osm::Way comb_way(osm::Way const& w1, osm::Way const& w2)
        {
            osm::Way ret(w1.id);
            ret.nodes=w2.nodes;
            ret.tags=w1.tags;
            return ret;
        }
    }
    
    class AllWayLister
    {
    public:
        typedef osm::LtByID way_comp;
        typedef decltype(util::sorted_combine(std::vector<osm::Way>(), std::vector<osm::Way>(), AllWayListerDetail::comb_way, osm::LtByID())) output_range_t;
        AllWayLister(OsmDatabase& db, std::multimap<std::string, std::string> const& attributes);
        output_range_t get_range();
    private:
        psql::Cursor<psql::BindTypes<>, psql::RetTypes<int64_t, std::string, std::string> > way_attr_crs;
        psql::Cursor<psql::BindTypes<>, psql::RetTypes<int64_t, int64_t, std::string, std::string> > way_node_attr_crs;
    };
}

#endif
