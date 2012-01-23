/*
 * ToShowEdgesSelector.h
 *
 *  Created on: Dec 29, 2011
 *      Author: martin
 */

#ifndef TOSHOWEDGESSELECTOR_H_
#define TOSHOWEDGESSELECTOR_H_

#include <memory>
#include "../displayer/LineDisplayStyle.h"
#include "../displayer/DisplayLine.h"

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
    static std::vector<std::unique_ptr<display::DisplayElement> > get_edges(psql::Statement < psql::BindTypes<Args...>,
            psql::RetTypes<double, double, double, double, double, double, double, double, double, int, int> > & st,
            Args... args)
    {
        std::vector<std::unique_ptr<display::DisplayElement> > ret;
        st.execute(args...);
        for (int i = 0; i < st.row_count(); ++i)
        {
            double lon1, lat1, lon2, lat2, r, g, b, a, t;
            int attrs, p;
            bool arrow = attrs & 1;
            std::tie(lon1, lat1, lon2, lat2, r, g, b, a, t, attrs, p) = st.get_row(i);
            ret.push_back(std::unique_ptr<display::DisplayElement>(
                              new display::DisplayLine(
                                  geo::Point(lat1, lon1),
                                  geo::Point(lat2, lon2),
                                  arrow,
                                  std::unique_ptr<display::DisplayStyle>(
                                      new display::LineDisplayStyle(r, g, b, a, t)
                                  ))));
        }
        return ret;
    }
};

} /* namespace osmdb */
#endif /* TOSHOWEDGESSELECTOR_H_ */
