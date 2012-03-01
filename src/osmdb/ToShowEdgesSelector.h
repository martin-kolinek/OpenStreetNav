/*
 * ToShowEdgesSelector.h
 *
 *  Created on: Dec 29, 2011
 *      Author: martin
 */

#ifndef TOSHOWEDGESSELECTOR_H_
#define TOSHOWEDGESSELECTOR_H_

#include <memory>
#include "../elements/osmelements.h"

namespace osmdb
{

class ToShowEdgesSelector
{
public:
    /**
     * Transforms st results into DisplayElements
     * @param st Statement returned by ToShowSelectCollection
     * @param args
     * @return vector of DisplayElements
     */
    template<typename... Args>
    static std::vector<std::pair<osm::Edge, display::LineDisplayStyle> > get_edges(psql::Statement < psql::BindTypes<Args...>,
            psql::RetTypes<int64_t, double, double, int64_t, double, double, int64_t, double, double, double, double, double, int, int> > & st,
            Args... args)
    {
        std::vector<std::pair<osm::Edge, display::LineDisplayStyle> > ret;
        st.execute(args...);
        for (int i = 0; i < st.row_count(); ++i)
        {
            double lon1, lat1, lon2, lat2, r, g, b, a, t;
            int64_t id1, id2, wid;
            int attrs, p;
            bool arrow = attrs & 1;
            std::tie(id1, lon1, lat1, id2, lon2, lat2, wid, r, g, b, a, t, attrs, p) = st.get_row(i);
            ret.push_back(std::make_pair(osm::Edge(osm::Node(id1, lat1, lon1), osm::Node(id2, lat2, lon2), osm::Way(wid)),
                                         display::LineDisplayStyle(r, g, b, a, t, arrow)));
        }
        return ret;
    }
};

} /* namespace osmdb */
#endif /* TOSHOWEDGESSELECTOR_H_ */
