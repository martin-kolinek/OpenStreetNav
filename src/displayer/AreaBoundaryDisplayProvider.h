/*
 * AreaBoundaryDisplayProvider.h
 *
 *  Created on: May 26, 2012
 *      Author: martin
 */

#ifndef AREABOUNDARYDISPLAYPROVIDER_H_
#define AREABOUNDARYDISPLAYPROVIDER_H_

#include "DisplayProvider.h"
#include "../geoelements/geoelements.h"

namespace display
{

class AreaBoundaryDisplayProvider : public DisplayProvider
{
public:
    AreaBoundaryDisplayProvider(std::shared_ptr<DisplayStyle> style);
    void set_boundary(std::vector<geo::Point> const& ps);
    element_range get_display_elements();
    void set_bounds(geo::Point const& topleft, geo::Point const& bottomright, int zoom);
    std::vector<std::shared_ptr<Descriptible> > get_selected(geo::Point const& topleft, geo::Point const& bottomright, int zoom);
    double center_lat();
    double center_lon();
    int get_min_zoom();
    int get_max_zoom();
    virtual ~AreaBoundaryDisplayProvider();
private:
    std::vector<std::shared_ptr<DisplayElement> > cache;
    std::shared_ptr<DisplayStyle> style;
    std::vector<geo::Edge> area;
    double top, bottom, left, right;
    void renew_cache();
    bool cache_ok;
};

} /* namespace display */
#endif /* AREABOUNDARYDISPLAYPROVIDER_H_ */
